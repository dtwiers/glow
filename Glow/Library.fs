module Glow

type StyleArray = byte array

type FormattingElement =
    | RawText of string
    | StyledText of StyledText

and StyledText = {
    Styles: StyleArray list
    FormattedElements: FormattingElement list
}

let Style styles formattedElements =
    StyledText {
        Styles = styles
        FormattedElements = formattedElements
    }
let Text (str: string) = RawText str

let Reset = [|0uy|]

module Font =

    let inline private toggleable toggle input = if toggle then [|input|] else [|20uy + input|]
    let Bold toggle = toggleable toggle 1uy
    let Dim toggle = toggleable toggle 2uy
    let Italic toggle = toggleable toggle 3uy
    let Underline toggle = toggleable toggle 4uy
    let Blink toggle = toggleable toggle 5uy
    let Inverse toggle = toggleable toggle 7uy
    let CrossedOut toggle = toggleable toggle 9uy

module Color =

    type AnsiColor =
        | Black
        | Red
        | Green
        | Yellow
        | Blue
        | Magenta
        | Cyan
        | White
        | BrightBlack
        | BrightRed
        | BrightGreen
        | BrightYellow
        | BrightBlue
        | BrightMagenta
        | BrightCyan
        | BrightWhite

    let private rawColorCode code =
        match code with
        | Black -> [|0uy|]
        | Red -> [|1uy |]
        | Green -> [|2uy|]
        | Yellow -> [|3uy|]
        | Blue -> [|4uy|]
        | Magenta -> [|5uy|]
        | Cyan -> [|6uy|]
        | White -> [|7uy|]
        | BrightBlack -> [|60uy|]
        | BrightRed -> [|61uy |]
        | BrightGreen -> [|62uy|]
        | BrightYellow -> [|63uy|]
        | BrightBlue -> [|64uy|]
        | BrightMagenta -> [|65uy|]
        | BrightCyan -> [|66uy|]
        | BrightWhite -> [|67uy|]

    let private toForeground color = color |> Array.map (fun x -> x + 30uy)
    let private toBackground color = color |> Array.map (fun x -> x + 40uy)

    type AnsiColorType =
        | Ansi of AnsiColor
        | From256 of byte
        | FromTrueColor of (byte * byte * byte)
    let Foreground color =
        match color with
        | None -> [|39uy|]
        | Some colorType ->
            match colorType with
            | Ansi c -> toForeground (rawColorCode c)
            | From256 code -> [|38uy; 5uy; code|]
            | FromTrueColor (r, g, b) -> [|38uy; 2uy; r; g; b|]

    let Background color =
        match color with
        | None -> [|49uy|]
        | Some colorType ->
            match colorType with
            | Ansi c -> toBackground (rawColorCode c)
            | From256 code -> [|48uy; 5uy; code|]
            | FromTrueColor (r, g, b) -> [|48uy; 2uy; r; g; b|]

type GlowSettings = {
    Enable: bool
}

let escape (codes: StyleArray list) =
    let flattened =
        let listOfList =
            codes |> List.map Array.toList
        listOfList |> List.concat
    sprintf "\x1b[%sm" (String.concat ";" (flattened |> List.map string))

let render settings glowText =
    let rec renderRecursive styleList element =
        match element with
        | RawText s ->
            if settings.Enable then
                sprintf "%s%s%s" (escape styleList) s (escape [Reset])
            else
                s
        | StyledText st ->
            st.FormattedElements
            |> List.map (fun x -> renderRecursive (st.Styles) x)
            |> String.concat ""
    renderRecursive [] glowText