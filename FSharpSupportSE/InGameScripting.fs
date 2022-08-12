module InGameScripting 

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open System.Text
open FSharp.Compiler.Diagnostics
open HarmonyLib
open Sandbox.Game.Entities.Blocks
open Sandbox.Game.Gui
open Sandbox.Game.Localization
open Sandbox.Game.Screens.Terminal.Controls
open FSharp.Compiler.Text
open Sandbox.Game.EntityComponents
 
open VRage.Game.Definitions
open Sandbox.Definitions

let fsharpModeGUID = Guid.Parse("8ee7af73-af43-40ca-b08c-0b4dc95a933b")

[<Literal>]
let noNamespace = """script must begin with "namespace FSharpSupport" """

//register guid to be able to serialize custom mod data in MyModStorageComponent
let definition = MyDefinitionManager.Static.GetEntityComponentDefinitions<MyModStorageComponentDefinition>().[0]
definition.RegisteredStorageGuids <- definition.RegisteredStorageGuids |> Array.append [|fsharpModeGUID|]
 
let (^) f x = f x

let log msg =
    FileLog.Log(DateTime.Now.ToString("[HH:mm:ss] ") + msg.ToString())

module Compilation =
    open FSharp.Compiler.CodeAnalysis
    let bin64Folder =
        Assembly.GetExecutingAssembly().Location
        |> Directory.GetParent
        |> fun x -> x.Parent.FullName
        
    let dependencies =
        [
            "Sandbox.Game.dll"
            "Sandbox.Common.dll"
            "Sandbox.Graphics.dll"
            "SpaceEngineers.Game.dll"
            "SpaceEngineers.ObjectBuilders.dll"
            "VRage.dll"
            "VRage.Audio.dll"
            "VRage.Game.dll"
            "VRage.Input.dll"
            "VRage.Library.dll"
            "VRage.Math.dll"
            "VRage.Render.dll"
            "VRage.Render11.dll"
            "VRage.Scripting.dll"
            "Plugins/FSharp.Core.dll"
        ]
        |> List.map (fun dll -> sprintf "%1s/%2s" bin64Folder dll )
        
    let fn = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
        
    let config file =
        { SourceFiles=[|file|];
          ConditionalCompilationDefines = FSharpParsingOptions.Default.ConditionalCompilationDefines;
          ErrorSeverityOptions = FSharpParsingOptions.Default.ErrorSeverityOptions;
          IsInteractive=false;
          LightSyntax= Some true;
          CompilingFsLib= false;
          IsExe=false
          LangVersionText = "F# 6.0.5" } 

    let mapErrors =
        Array.map ^ fun (x: FSharpDiagnostic) ->
            match x.Severity with
            | FSharpDiagnosticSeverity.Error -> VRage.Scripting.Message(true, x.Message)
            | _ -> VRage.Scripting.Message(false, x.Message)
        >> ResizeArray

     
        
    let hasNamespace (str: String) =
        str.TrimStart().StartsWith("namespace FSharpSupport")
            
    let compile assemblyName script : Result<VRage.Scripting.Message List * Assembly, VRage.Scripting.Message List> Async  =
        async {
            let checker = FSharpChecker.Create()
            let! res = checker.ParseFile(fn, SourceText.ofString script, config fn)
            if not ^ hasNamespace script then
                return Error (ResizeArray([VRage.Scripting.Message(true, noNamespace)]))
            else if not res.ParseHadErrors then
                let! errors, _, asm = checker.CompileToDynamicAssembly([res.ParseTree], assemblyName, dependencies, None, noframework = false)
                match asm with
                | Some asm -> return Ok(mapErrors errors, asm)
                | None -> return Error(mapErrors errors)
            else return Error (mapErrors res.Diagnostics)
        }
        
let joinErrors (errors: VRage.Scripting.Message seq) =
        errors
        |> Seq.map (fun x -> x.Text)
        |> fun x -> x.Join(delimiter=";")
        
let isModeEnabled (block: MyProgrammableBlock ): bool =
        if block.Storage = null then
            block.Storage <- MyModStorageComponent()
        match block.Storage.TryGetValue(fsharpModeGUID) with
        | true, v -> v = bool.TrueString
        | false, _ -> block.Storage.Add(fsharpModeGUID, bool.FalseString); false

[<HarmonyPatch(typeof<MyProgrammableBlock>, "Compile")>]
type CompilePatch =
    static member isModeEnabledStub(block: MyProgrammableBlock): bool = isModeEnabled block
    static member CompileFSharp
        (_ : VRage.Scripting.IVRageScripting,
         assemblyName: string,
         program: string,
         diagnostics: List<VRage.Scripting.Message> outref,
         _ : String,
         _ : String,
         _ : String) : Assembly =
        let result = Compilation.compile assemblyName program |> Async.RunSynchronously
        match result with
        | Error list ->
            //diagnostics <- list; log ^ "compilation FAILED! "; Seq.iter log list; null
            raise(Exception(joinErrors list))
        | Ok (list, asm ) ->
            diagnostics <- list; log ^ "compilation SUCCESS! " + program; asm
    (*
        This code emits if statement to replace C# compilng task to F# compile function
    *)
    [<HarmonyTranspiler>]
    static member Transpile(instructions: CodeInstruction seq, ilgenerator: ILGenerator): CodeInstruction seq = 
        seq{ 
            let jumpLabel = ilgenerator.DefineLabel()
            let jump2Label = ilgenerator.DefineLabel()
            let isModeEnableInfo = typeof<CompilePatch>.GetMethod "isModeEnabledStub"
            let compileFSharpInfo = typeof<CompilePatch>.GetMethod "CompileFSharp"
            let compileInGameScriptAsyncInfo = typeof<VRage.Scripting.IVRageScriptingFunctions>.GetMethod "CompileIngameScriptAsync"
            let mAssemblyInfo = AccessTools.Field(typeof<MyProgrammableBlock>, "m_assembly")
            let isFsharpEnabledIndex = ilgenerator.DeclareLocal(typeof<bool>).LocalIndex
            CodeInstruction(OpCodes.Ldarg_0) 
            CodeInstruction(OpCodes.Call, isModeEnableInfo)  
            CodeInstruction(OpCodes.Stloc, isFsharpEnabledIndex)  
            for instruction in instructions do
                if instruction.Calls compileInGameScriptAsyncInfo then
                     CodeInstruction(OpCodes.Ldloc, isFsharpEnabledIndex) 
                     CodeInstruction(OpCodes.Brtrue, jumpLabel) 
                     instruction  
                elif instruction.opcode = OpCodes.Stfld && instruction.operand = (mAssemblyInfo :> obj) then 
                     CodeInstruction(OpCodes.Br, jump2Label)  
                     CodeInstruction(OpCodes.Call, compileFSharpInfo) .WithLabels jumpLabel 
                     instruction.WithLabels jump2Label 
                else instruction
        }
    static member Postfix(__instance: MyProgrammableBlock) =
        let asm = AccessTools.Field(typeof<MyProgrammableBlock>,"m_assembly")
        let errors = AccessTools.Field(typeof<MyProgrammableBlock>, "m_compilerErrors")
        log "Compilation has passed"
        log ^ "Assembly is " + string (asm.GetValue(__instance))
        log ^ "Compiler errors is: "
        errors.GetValue __instance :?> _ seq |> Seq.iter log
        

[<HarmonyPatch(typeof<MyProgrammableBlock>, "CreateInstance")>]
type CreateInstancePatch =   
    [<HarmonyTranspiler>]
    static member Transpile(instructions: CodeInstruction seq, ilgenerator: ILGenerator) : CodeInstruction seq = 
        seq {
            let jumpLabel = ilgenerator.DefineLabel()
            let jump2Label = ilgenerator.DefineLabel()
            let isModeEnableInfo = typeof<CompilePatch>.GetMethod "isModeEnabledStub"
            for instruction in instructions do 
                if instruction.opcode = OpCodes.Ldstr && instruction.operand = ("Program" :> obj) then
                    CodeInstruction(OpCodes.Ldarg_0)
                    CodeInstruction(OpCodes.Call, isModeEnableInfo)
                    CodeInstruction(OpCodes.Brtrue, jumpLabel)
                    instruction
                    CodeInstruction(OpCodes.Br, jump2Label)
                    CodeInstruction(OpCodes.Ldstr, "FSharpSupport.Program") .WithLabels jumpLabel
                    CodeInstruction(OpCodes.Nop) .WithLabels jump2Label
                else instruction
        }

type CheckboxGetter = ``MyTerminalValueControl`2``.GetterDelegate<MyProgrammableBlock,bool>
type CheckboxSetter = ``MyTerminalValueControl`2``.SetterDelegate<MyProgrammableBlock,bool>

[<HarmonyPatch(typeof<MyProgrammableBlock>, "CreateTerminalControls")>]
type CreateTerminalControlsPatch =
    [<HarmonyPostfix>]
    static member Postfix() =
        let onCheckboxClick (block: MyProgrammableBlock) (v: bool) =
            if block.Storage <> null then
                match block.Storage.TryGetValue(fsharpModeGUID) with
                | true, _ -> block.Storage.SetValue(fsharpModeGUID, v.ToString())
                | false, _ -> block.Storage.Add(fsharpModeGUID, v.ToString())
        let array = ResizeArray()
        MyTerminalControlFactory.GetControls(typeof<MyProgrammableBlock>, array)
        match array |> Seq.tryFind(fun x -> x.Id = "F# mode") with
        | Some _ -> () 
        | None -> 
            let checkbox = MyTerminalControlCheckbox("F# mode", MySpaceTexts.Align_Left, MySpaceTexts.Blank)
            checkbox.Getter <- CheckboxGetter isModeEnabled
            checkbox.Enabled <- Func<MyProgrammableBlock, bool> (fun _ -> true)
            checkbox.Setter <- CheckboxSetter onCheckboxClick                        
            MyTerminalControlFactory.AddControl<MyProgrammableBlock>(0, checkbox)
        