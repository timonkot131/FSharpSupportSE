namespace FSharpSupportSE

open VRage
open VRage.Scripting
open VRage.Plugins
open System.IO
open System
open HarmonyLib
open Sandbox.Definitions
open VRage.Game

type Main() =
     interface IPlugin with
        member _.Dispose(): unit = ()
        member _.Init _ : unit =
            FileLog.logPath <- "/home/timonkot13/Harmonylogs.txt"
            let harmony = Harmony "FSharpSupportSE"
            harmony.PatchAll() 
        member _.Update() = ()