[<AutoOpen>]
module internal Dyfrig.Http.Formatters

(* RFC 7231 *)

let formatMethod =
    function | DELETE -> "DELETE" 
             | HEAD -> "HEAD" 
             | GET -> "GET" 
             | OPTIONS -> "OPTIONS"
             | PATCH -> "PATCH" 
             | POST -> "POST" 
             | PUT -> "PUT"  
             | TRACE -> "TRACE"
             | Method.Custom x -> x

let formatProtocol =
    function | Protocol.HTTP x -> sprintf "HTTP/%.2g" x 
             | Protocol.Custom x -> x

let formatScheme =    
    function | HTTP -> "http"
             | HTTPS -> "https" 
             | Scheme.Custom x -> x

let formatQuery m =
    Map.toArray m
    |> Array.map (fun (x, y) -> sprintf "%s=%s" x y)
    |> Array.rev
    |> String.concat "&"

let private formatWeight =
    Option.map (sprintf ";q=%.4g") >> Option.getOrElse ""

let formatAccept =
    List.map (fun (x) ->
        let mediaRange =
            match x.MediaRange with
            | MediaRange.Specified (Closed (MediaType x, MediaSubType y)) -> sprintf "%s/%s" x y
            | MediaRange.Partial (Partial (MediaType x)) -> sprintf "%s/*" x
            | MediaRange.Partial (Open) -> "*/*"

        let mediaRangeParameters =
            match x.MediaRangeParameters.Count with
            | 0 -> ""
            | _ ->
                x.MediaRangeParameters
                |> Map.toArray
                |> Array.map (fun (x, y) -> sprintf "%s=%s" x y)
                |> Array.rev
                |> String.concat ";"

        sprintf "%s%s%s" mediaRange mediaRangeParameters (formatWeight x.Weight))
    >> String.concat ","

let formatAcceptCharset =
    List.map (fun x ->
        let charset =
            match x.Charset with
            | Charset.Specified (SpecifiedCharset.Named x) -> x
            | Charset.Any -> "*"                    

        sprintf "%s%s" charset (formatWeight x.Weight))
    >> String.concat ","

let formatAcceptEncoding =
    List.map (fun x ->
        let encoding =
            match x.Encoding with
            | Encoding.Specified (SpecifiedEncoding.Named x) -> x
            | Encoding.Specified (SpecifiedEncoding.Identity) -> "identity"
            | Encoding.Any -> "*"                    

        sprintf "%s%s" encoding (formatWeight x.Weight)) 
    >> String.concat ","

let formatAcceptLanguage =
    List.map (fun x -> sprintf "%s%s" x.Language.Name (formatWeight x.Weight))
    >> String.concat ","

(* RFC 7232 *)

let formatETag =
    function | Strong x -> sprintf "\"%s\"" x
             | Weak x -> sprintf "W/\"%s\"" x

let formatIfMatch =
    function | IfMatch.EntityTags x -> List.map formatETag x |> String.concat ","
             | IfMatch.Any -> "*"

let formatIfNoneMatch =
    function | IfNoneMatch.EntityTags x -> List.map formatETag x |> String.concat ","
             | IfNoneMatch.Any -> "*"