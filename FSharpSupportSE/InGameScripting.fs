module InGameScripting 

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Reflection.Emit
open HarmonyLib
open Sandbox.Game.Entities.Blocks
open Sandbox.Game.Gui
open Sandbox.Game.Localization
open Sandbox.Game.Screens.Terminal.Controls
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Sandbox.Game.EntityComponents

let fsharpModeGUID = Guid.Parse("8ee7af73-af43-40ca-b08c-0b4dc95a933b")

[<Literal>]
let fsharpMode = "__FSHARP_MODE__"

let fn = Path.ChangeExtension(Path.GetTempFileName(), ".fs")

let dependencies =
        [
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\Sandbox.Game.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\Sandbox.Common.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\Sandbox.Graphics.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\SpaceEngineers.Game.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\SpaceEngineers.ObjectBuilders.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Audio.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Game.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Input.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Library.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Math.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Render.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Render11.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\VRage.Scripting.dll"
            "D:\Program Files (x86)\Steam\steamapps\common\SpaceEngineers\Bin64\Plugins\FSharp.Core.dll"
        ] 

let config file =
    {SourceFiles=[|file|];
     ConditionalCompilationDefines = FSharpParsingOptions.Default.ConditionalCompilationDefines;
     ErrorSeverityOptions = FSharpParsingOptions.Default.ErrorSeverityOptions;
     IsInteractive=false;
     LightSyntax= Some true;
     CompilingFsLib= false;
     IsExe=false} 

let rec isModeEnabled (block: MyProgrammableBlock ): bool = 
    if block.Storage <> null then
        match block.Storage.TryGetValue(fsharpModeGUID) with
        | true, v -> v = bool.TrueString
        | false, _ -> block.Storage.Add(fsharpModeGUID, bool.FalseString); false
    else block.Storage <- MyModStorageComponent(); isModeEnabled(block)

let mapErrors =
    Array.map <| fun (x: FSharpErrorInfo) ->
        match x.Severity with
        | FSharpErrorSeverity.Warning -> VRage.Scripting.Message(false, x.Message)
        | FSharpErrorSeverity.Error -> VRage.Scripting.Message(true, x.Message)
    >> ResizeArray

let compile assemblyName script : Result<VRage.Scripting.Message List * Assembly, VRage.Scripting.Message List> Async  =
    async {
          let checker = FSharpChecker.Create()
          let! res = checker.ParseFile(fn, SourceText.ofString script, config fn)
          match res.ParseTree with
          | Some tree -> 
              let! (errors, _, asm) =
                   checker.CompileToDynamicAssembly([tree], assemblyName, dependencies, None, noframework=false)
              match asm with
              | Some asm -> return Ok (mapErrors errors, asm)
              | None -> return Error (mapErrors errors)
          | None -> return Error (mapErrors res.Errors)
      }

[<HarmonyPatch(typeof<MyProgrammableBlock>, "Compile")>]
type MyProgrammableBlockCompile =
    static member isModeEnabledStub(block: MyProgrammableBlock): bool = isModeEnabled block

    static member CompileFSharp
        (_ : VRage.Scripting.IVRageScripting,
         assemblyName: string,
         program: string,
         diagnostics: List<VRage.Scripting.Message> outref,
         _ : String,
         _ : String,
         _ : String) : Assembly =
        let result = compile assemblyName program |> Async.RunSynchronously
        match result with
        | Error list -> diagnostics <- list; null
        | Ok (list, asm ) -> diagnostics <- list; asm

    (*
        This code emits if statement to replace C# compilng task to F# compile function
    *)
    [<HarmonyTranspiler>]
    static member Transpile(instuctions: CodeInstruction seq, ilgenerator: ILGenerator): CodeInstruction seq = 
        seq{
            let jumpLabel = ilgenerator.DefineLabel()
            let jump2Label = ilgenerator.DefineLabel()
            let isModeEnableInfo = typeof<MyProgrammableBlockCompile>.GetMethod "isModeEnabledStub"
            let compileFSharpInfo = typeof<MyProgrammableBlockCompile>.GetMethod "CompileFSharp"
            let compileInGameScriptAsyncInfo = typeof<VRage.Scripting.IVRageScriptingFunctions>.GetMethod "CompileIngameScriptAsync"
            let mAssemblyInfo = AccessTools.Field(typeof<MyProgrammableBlock>, "m_assembly")
            let isFsharpEnabledIndex = ilgenerator.DeclareLocal(typeof<bool>).LocalIndex
            CodeInstruction(OpCodes.Ldarg_0) 
            CodeInstruction(OpCodes.Call, isModeEnableInfo)  
            CodeInstruction(OpCodes.Stloc, isFsharpEnabledIndex)  
            for instuction in instuctions do
                if instuction.Calls compileInGameScriptAsyncInfo then
                     CodeInstruction(OpCodes.Ldloc, isFsharpEnabledIndex) 
                     CodeInstruction(OpCodes.Brtrue, jumpLabel) 
                     instuction  
                elif instuction.opcode = OpCodes.Stfld && instuction.operand = (mAssemblyInfo :> obj) then 
                     CodeInstruction(OpCodes.Br, jump2Label)  
                     CodeInstruction(OpCodes.Call, compileFSharpInfo) .WithLabels jumpLabel 
                     instuction.WithLabels jump2Label 
                else instuction 
        }

[<HarmonyPatch(typeof<MyProgrammableBlock>, "CreateInstance")>]
type CreateInstancePatch =
    [<HarmonyTranspiler>]
    static member Transpile(instructions: CodeInstruction seq, ilgenerator: ILGenerator) : CodeInstruction seq = 
        seq {
            let jumpLabel = ilgenerator.DefineLabel()
            let jump2Label = ilgenerator.DefineLabel()
            let isModeEnableInfo = typeof<MyProgrammableBlockCompile>.GetMethod "isModeEnabledStub"
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
        let array = ResizeArray()
        MyTerminalControlFactory.GetControls(typeof<MyProgrammableBlock>, array)
        match array |> Seq.tryFind(fun x -> x.Id = "F# mode") with
        | Some _ -> () 
        | None -> 
            let checkbox = MyTerminalControlCheckbox("F# mode", MySpaceTexts.Align_Left, MySpaceTexts.Blank)
            checkbox.Getter <- CheckboxGetter isModeEnabled
            checkbox.Enabled <- Func<MyProgrammableBlock, bool> (fun _ -> true)
            checkbox.Setter <- CheckboxSetter (fun (block) v ->
                let rec setValue(block: MyProgrammableBlock) =
                    if block.Storage <> null then
                        match block.Storage.TryGetValue(fsharpModeGUID) with
                        | true, _ -> block.Storage.SetValue(fsharpModeGUID, v.ToString())
                        | false, _ -> block.Storage.Add(fsharpModeGUID, v.ToString()) 
                    else block.Storage <- MyModStorageComponent(); setValue(block)
                setValue(block))
            MyTerminalControlFactory.AddControl<MyProgrammableBlock>(0, checkbox)
        