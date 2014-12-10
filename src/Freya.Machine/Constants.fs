[<AutoOpen>]
module internal Freya.Machine.Constants


[<RequireQualifiedAccess>]
module Actions =

    let [<Literal>] private prefix = "do"

    let [<Literal>] Delete = prefix + "Delete"
    let [<Literal>] Patch = prefix + "Patch"
    let [<Literal>] Post = prefix + "Post"
    let [<Literal>] Put = prefix + "Put"


[<RequireQualifiedAccess>]
module Configuration =

    let [<Literal>] CharsetsSupported = "charsetsSupported"
    let [<Literal>] CorsHeadersExposed = "corsHeadersExposed"
    let [<Literal>] CorsHeadersSupported = "corsHeadersSupported"
    let [<Literal>] CorsMethodsSupported = "corsMethodsSupported"
    let [<Literal>] CorsOriginsSupported = "corsOriginsSupported"
    let [<Literal>] EncodingsSupported = "encodingsSupported"
    let [<Literal>] ETag = "etag"
    let [<Literal>] LanguagesSupported = "languagesSupported"
    let [<Literal>] LastModified = "lastModified"
    let [<Literal>] MediaTypesSupported = "mediaTypesSupported"
    let [<Literal>] MethodsSupported = "methodsSupported"
    let [<Literal>] MethodsKnown = "methodsKnown"


[<RequireQualifiedAccess>]
module Decisions =

    (* Public *)

    let [<Literal>] Allowed = "allowed"
    let [<Literal>] AllowPostToGone = "allowPostToGone"
    let [<Literal>] AllowPostToMissing = "allowPostToMissing"
    let [<Literal>] AllowPutToMissing = "allowPutToMissing"
    let [<Literal>] Authorized = "authorized"
    let [<Literal>] CharsetsStrict = "charsetsStrict"
    let [<Literal>] Conflicts = "conflicts"
    let [<Literal>] ContentTypeKnown = "contentTypeKnown"
    let [<Literal>] ContentTypeValid = "contentTypeValid"
    let [<Literal>] Created = "created"
    let [<Literal>] Deleted = "deleted"
    let [<Literal>] EncodingsStrict = "encodingsStrict"
    let [<Literal>] EntityLengthValid = "entityLengthValid"
    let [<Literal>] Existed = "existed"
    let [<Literal>] Exists = "exists"
    let [<Literal>] LanguagesStrict = "languagesStrict"
    let [<Literal>] Malformed = "malformed"
    let [<Literal>] MediaTypesStrict = "mediaTypesStrict"
    let [<Literal>] MovedPermanently = "movedPermanently"
    let [<Literal>] MovedTemporarily = "movedTemporarily"
    let [<Literal>] MultipleRepresentations = "multipleRepresentations"
    let [<Literal>] PostRedirect = "postRedirect"
    let [<Literal>] Processable = "processable"
    let [<Literal>] PutToDifferentUri = "putToDifferentUri"
    let [<Literal>] RespondWithEntity = "respondWithEntity"
    let [<Literal>] ServiceAvailable = "serviceAvailable"
    let [<Literal>] UriTooLong = "uriTooLong"

    (* Internal *)

    let [<Literal>] CharsetNegotiable = "charsetNegotiable"
    let [<Literal>] CharsetRequested = "charsetRequested"
    let [<Literal>] CorsEnabled = "corsEnabled"
    let [<Literal>] CorsOptions = "corsOptions"
    let [<Literal>] CorsOrigin = "corsOrigin"
    let [<Literal>] CorsPreflight = "corsPreflight"
    let [<Literal>] EncodingNegotiable = "encodingNegotiable"
    let [<Literal>] EncodingRequested = "encodingRequested"
    let [<Literal>] IfMatchAny = "ifMatchAny"
    let [<Literal>] IfMatchExistsForMissing = "ifMatchExistsForMissing"
    let [<Literal>] IfMatchRequested = "ifMatchRequested"
    let [<Literal>] IfModifiedSinceModified = "ifModifiedSinceModified"
    let [<Literal>] IfModifiedSinceRequested = "ifModifiedSinceRequested"
    let [<Literal>] IfModifiedSinceValid = "ifModifiedSinceValid"
    let [<Literal>] IfNoneMatchAny = "ifNoneMatchAny"
    let [<Literal>] IfNoneMatchRequested = "ifNoneMatchRequested"
    let [<Literal>] IfUnmodifiedSinceModified = "ifUnmodifiedSinceModified"
    let [<Literal>] IfUnmodifiedSinceRequested = "ifUnmodifiedSinceRequested"
    let [<Literal>] IfUnmodifiedSinceValid = "ifUmodifiedSinceValid"
    let [<Literal>] LanguageNegotiable = "languageNegotiable"
    let [<Literal>] LanguageRequested = "languageRequested"
    let [<Literal>] MediaTypeNegotiable = "mediaTypeNegotiable"
    let [<Literal>] MediaTypeRequested = "mediaTypeRequested"
    let [<Literal>] MethodDelete = "methodDelete"
    let [<Literal>] MethodGetOrHead = "methodGetOrHead"
    let [<Literal>] MethodKnown = "methodKnown"
    let [<Literal>] MethodOptions = "methodOptions"
    let [<Literal>] MethodPatch = "methodPatch"
    let [<Literal>] MethodPostToExisting = "methodPostToExisting"
    let [<Literal>] MethodPostToGone = "methodPostToGone"
    let [<Literal>] MethodPostToMissing = "methodPostToMissing"
    let [<Literal>] MethodPut = "methodPut"
    let [<Literal>] MethodPutToExisting = "methodPutToExisting"
    let [<Literal>] MethodSupported = "methodSupported"

    let [<Literal>] ETagMatchesIf = "resourceETagMatchesIf"
    let [<Literal>] ETagMatchesIfNone = "resourceETagMatchesIfNone"


[<RequireQualifiedAccess>]
module Handlers =

    let [<Literal>] private prefix = "handle"

    (* 200 *)

    let [<Literal>] OK = prefix + "Ok"
    let [<Literal>] Created = prefix + "Created"
    let [<Literal>] Options = prefix + "Options"
    let [<Literal>] Accepted = prefix + "Accepted"
    let [<Literal>] NoContent = prefix + "NoContent"

    (* 300 *)

    let [<Literal>] MovedPermanently = prefix + "MovedPermanently"
    let [<Literal>] SeeOther = prefix + "SeeOther"
    let [<Literal>] NotModified = prefix + "NotModified"
    let [<Literal>] MovedTemporarily = prefix + "MovedTemporarily"
    let [<Literal>] MultipleRepresentations = prefix + "MultipleRepresentations"

    (* 400 *)

    let [<Literal>] Malformed = prefix + "Malformed"
    let [<Literal>] Unauthorized = prefix + "Unauthorized"
    let [<Literal>] Forbidden = prefix + "Forbidden"
    let [<Literal>] NotFound = prefix + "NotFound"
    let [<Literal>] MethodNotAllowed = prefix + "MethodNotAllowed"
    let [<Literal>] NotAcceptable = prefix + "NotAcceptable"
    let [<Literal>] Conflict = prefix + "Conflict"
    let [<Literal>] Gone = prefix + "Gone"
    let [<Literal>] PreconditionFailed = prefix + "PreconditionFailed"
    let [<Literal>] RequestEntityTooLarge = prefix + "RequestEntityTooLarge"
    let [<Literal>] UriTooLong = prefix + "UriTooLong"
    let [<Literal>] UnsupportedMediaType = prefix + "UnsupportedMediaType"
    let [<Literal>] UnprocessableEntity = prefix + "UnprocessableEntity"

    (* 500 *)

    let [<Literal>] NotImplemented = prefix + "NotImplemented"
    let [<Literal>] UnknownMethod = prefix + "UnknownMethod"
    let [<Literal>] ServiceUnavailable = prefix + "ServiceUnavailable"


[<RequireQualifiedAccess>]
module Operations =

    let [<Literal>] private prefix = "set"

    (* 200 *)

    let [<Literal>] SetOK = prefix + "Ok"
    let [<Literal>] SetCreated = prefix + "Created"
    let [<Literal>] SetOptions = prefix + "Options"
    let [<Literal>] SetAccepted = prefix + "Accepted"
    let [<Literal>] SetNoContent = prefix + "NoContent"

    (* 300 *)

    let [<Literal>] SetMovedPermanently = prefix + "MovedPermanently"
    let [<Literal>] SetSeeOther = prefix + "SeeOther"
    let [<Literal>] SetNotModified = prefix + "NotModified"
    let [<Literal>] SetMovedTemporarily = prefix + "MovedTemporarily"
    let [<Literal>] SetMultipleRepresentations = prefix + "MultipleRepresentations"

    (* 400 *)

    let [<Literal>] SetMalformed = prefix + "Malformed"
    let [<Literal>] SetUnauthorized = prefix + "Unauthorized"
    let [<Literal>] SetForbidden = prefix + "Forbidden"
    let [<Literal>] SetNotFound = prefix + "NotFound"
    let [<Literal>] SetMethodNotAllowed = prefix + "MethodNotAllowed"
    let [<Literal>] SetNotAcceptable = prefix + "NotAcceptable"
    let [<Literal>] SetConflict = prefix + "Conflict"
    let [<Literal>] SetGone = prefix + "Gone"
    let [<Literal>] SetPreconditionFailed = prefix + "PreconditionFailed"
    let [<Literal>] SetRequestEntityTooLarge = prefix + "RequestEntityTooLarge"
    let [<Literal>] SetUriTooLong = prefix + "UriTooLong"
    let [<Literal>] SetUnsupportedMediaType = prefix + "UnsupportedMediaType"
    let [<Literal>] SetUnprocessableEntity = prefix + "UnprocessableEntity"

    (* 500 *)

    let [<Literal>] SetNotImplemented = prefix + "NotImplemented"
    let [<Literal>] SetUnknownMethod = prefix + "UnknownMethod"
    let [<Literal>] SetServiceUnavailable = prefix + "ServiceUnavailable"

    (* CORS *)

    let [<Literal>] SetCorsPreflight = prefix + "CorsPreflight"
    let [<Literal>] SetCorsActual = prefix + "CorsActual"
    let [<Literal>] SetCorsOrigin = prefix + "CorsOrigin"