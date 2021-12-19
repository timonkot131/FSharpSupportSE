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
        member _.Init(gameInstance: obj): unit = 
            let harmony = Harmony("FSharpSupportSE")
            harmony.PatchAll() 
        member _.Update() = ()