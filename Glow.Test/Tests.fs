module Tests

open Expecto
open Glow
open Glow.Color

[<Tests>]
let glowTests =
    testList "Glow" [
        testCase "First Test" <| fun _ ->
            let testGlow =
                Style [
                    Font.Bold true
                    Font.Underline true
                    Foreground (Some (Ansi Cyan))
                ] [
                    RawText "Bold Underlined Cyan\n"
                    Style [Foreground (Some (From256 202uy)); Font.Inverse true] [RawText "Red\n"]
                    RawText "Back to Underlined Bold Cyan"
                ]
                |> render { Enable = true }
            Expect.contains testGlow '\x1b' "Should contain the magic escape character"
    ]
