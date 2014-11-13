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
    let [<Literal>] Conflicts = "conflicts"
    let [<Literal>] ContentTypeKnown = "contentTypeKnown"
    let [<Literal>] ContentTypeValid = "contentTypeValid"
    let [<Literal>] Created = "created"
    let [<Literal>] Deleted = "deleted"
    let [<Literal>] EntityLengthValid = "entityLengthValid"
    let [<Literal>] Existed = "existed"
    let [<Literal>] Exists = "exists"
    let [<Literal>] Malformed = "malformed"
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

    let [<Literal>] Exception = prefix + "Exception"
    let [<Literal>] NotImplemented = prefix + "NotImplemented"
    let [<Literal>] UnknownMethod = prefix + "UnknownMethod"
    let [<Literal>] ServiceUnavailable = prefix + "ServiceUnavailable"


[<RequireQualifiedAccess>]
module Operations =

    let [<Literal>] private prefix = "pre"

    (* 200 *)

    let [<Literal>] PreOK = prefix + "Ok"
    let [<Literal>] PreCreated = prefix + "Created"
    let [<Literal>] PreOptions = prefix + "Options"
    let [<Literal>] PreAccepted = prefix + "Accepted"
    let [<Literal>] PreNoContent = prefix + "NoContent"

    (* 300 *)

    let [<Literal>] PreMovedPermanently = prefix + "MovedPermanently"
    let [<Literal>] PreSeeOther = prefix + "SeeOther"
    let [<Literal>] PreNotModified = prefix + "NotModified"
    let [<Literal>] PreMovedTemporarily = prefix + "MovedTemporarily"
    let [<Literal>] PreMultipleRepresentations = prefix + "MultipleRepresentations"

    (* 400 *)

    let [<Literal>] PreMalformed = prefix + "Malformed"
    let [<Literal>] PreUnauthorized = prefix + "Unauthorized"
    let [<Literal>] PreForbidden = prefix + "Forbidden"
    let [<Literal>] PreNotFound = prefix + "NotFound"
    let [<Literal>] PreMethodNotAllowed = prefix + "MethodNotAllowed"
    let [<Literal>] PreNotAcceptable = prefix + "NotAcceptable"
    let [<Literal>] PreConflict = prefix + "Conflict"
    let [<Literal>] PreGone = prefix + "Gone"
    let [<Literal>] PrePreconditionFailed = prefix + "PreconditionFailed"
    let [<Literal>] PreRequestEntityTooLarge = prefix + "RequestEntityTooLarge"
    let [<Literal>] PreUriTooLong = prefix + "UriTooLong"
    let [<Literal>] PreUnsupportedMediaType = prefix + "UnsupportedMediaType"
    let [<Literal>] PreUnprocessableEntity = prefix + "UnprocessableEntity"

    (* 500 *)

    let [<Literal>] PreException = prefix + "Exception"
    let [<Literal>] PreNotImplemented = prefix + "NotImplemented"
    let [<Literal>] PreUnknownMethod = prefix + "UnknownMethod"
    let [<Literal>] PreServiceUnavailable = prefix + "ServiceUnavailable"
