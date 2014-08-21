namespace Dyfrig.Machine

[<RequireQualifiedAccess>]
module internal Actions =

    let [<Literal>] private prefix = "do"

    let [<Literal>] Delete = prefix + "Delete"
    let [<Literal>] Patch = prefix + "Patch"
    let [<Literal>] Post = prefix + "Post"
    let [<Literal>] Put = prefix + "Put"

[<RequireQualifiedAccess>]
module internal Config =
    
    let [<Literal>] AllowedMethods = "allowedMethods"
    let [<Literal>] AvailableCharsets = "availableCharsets"
    let [<Literal>] AvailableEncodings = "availableEncodings"
    let [<Literal>] AvailableLanguages = "availableLanguages"
    let [<Literal>] AvailableMediaTypes = "availableMediaTypes"
    let [<Literal>] ETag = "eTag"
    let [<Literal>] KnownMethods = "knownMethods"
    let [<Literal>] Modified = "modified"

[<RequireQualifiedAccess>]
module internal Decisions =

    // Public

    let [<Literal>] Allowed = "allowed"
    let [<Literal>] Authorized = "authorized"
    let [<Literal>] CanPostToGone = "canPostToGone"
    let [<Literal>] CanPostToMissing = "canPostToMissing"
    let [<Literal>] CanPutToMissing = "canPutToMissing"
    let [<Literal>] CharsetAvailable = "charsetAvailable"
    let [<Literal>] Conflict = "conflict"
    let [<Literal>] ContentTypeKnown = "contentTypeKnown"
    let [<Literal>] ContentTypeValid = "contentTypeValid"
    let [<Literal>] Created = "created"
    let [<Literal>] Deleted = "deleted"
    let [<Literal>] EncodingAvailable = "encodingAvailable"
    let [<Literal>] ETagMatchesIf = "eTagMatchesForIf"
    let [<Literal>] ETagMatchesIfNone = "eTagMatchesForIfNone"
    let [<Literal>] Existed = "existed"
    let [<Literal>] Exists = "exists"
    let [<Literal>] LanguageAvailable = "languageAvailable"
    let [<Literal>] Malformed = "malformed"
    let [<Literal>] MediaTypeAvailable = "mediaTypeAvailable"
    let [<Literal>] MethodAllowed = "methodAllowed"
    let [<Literal>] MethodKnown = "methodKnown"
    let [<Literal>] ModifiedSince = "modifiedSince"
    let [<Literal>] MovedPermanently = "movedPermanently"
    let [<Literal>] MovedTemporarily = "movedTemporarily"
    let [<Literal>] MultipleRepresentations = "multipleRepresentations"
    let [<Literal>] PostRedirect = "postRedirect"
    let [<Literal>] Processable = "processable"
    let [<Literal>] PutToDifferentUri = "putToDifferentUri"
    let [<Literal>] RespondWithEntity = "respondWithEntity"
    let [<Literal>] ServiceAvailable = "serviceAvailable"
    let [<Literal>] UnmodifiedSince = "unmodifiedSince"
    let [<Literal>] UriTooLong = "uriTooLong"
    let [<Literal>] ValidEntityLength = "validEntityLength"

    // Internal
    
    let [<Literal>] AcceptCharsetExists = "acceptCharsetExists"
    let [<Literal>] AcceptEncodingExists = "acceptEncodingExists"
    let [<Literal>] AcceptExists = "acceptExists"
    let [<Literal>] AcceptLanguageExists = "acceptLanguageExists"
    let [<Literal>] IfMatchExists = "ifMatchExists"
    let [<Literal>] IfMatchStar = "ifMatchStar"
    let [<Literal>] IfMatchStarExistsForMissing = "ifMatchStarExistsForMissing"
    let [<Literal>] IfModifiedSinceExists = "ifModifiedSinceExists"
    let [<Literal>] IfModifiedSinceValidDate = "ifModifiedSinceValidDate"
    let [<Literal>] IfNoneMatch = "ifNoneMatch"
    let [<Literal>] IfNoneMatchExists = "ifNoneMatchExists"
    let [<Literal>] IfNoneMatchStar = "ifNoneMatchStar"
    let [<Literal>] IfUnmodifiedSinceExists = "ifUnmodifiedSinceExists"
    let [<Literal>] IfUnmodifiedSinceValidDate = "ifUmodifiedSinceValidDate"
    let [<Literal>] MethodDelete = "methodDelete"
    let [<Literal>] MethodOptions = "methodOptions"
    let [<Literal>] MethodPatch = "methodPatch"
    let [<Literal>] MethodPut = "methodPut"
    let [<Literal>] PostToGone = "postToGone"
    let [<Literal>] PostToExisting = "postToExisting"
    let [<Literal>] PostToMissing = "postToMissing"
    let [<Literal>] PutToExisting = "putToExisting"

[<RequireQualifiedAccess>]
module internal Handlers =

    let [<Literal>] private prefix = "handle"

    // 200

    let [<Literal>] OK = prefix + "Ok"
    let [<Literal>] Created = prefix + "Created"
    let [<Literal>] Options = prefix + "Options"
    let [<Literal>] Accepted = prefix + "Accepted"
    let [<Literal>] NoContent = prefix + "NoContent"

    // 300

    let [<Literal>] MovedPermanently = prefix + "MovedPermanently"
    let [<Literal>] SeeOther = prefix + "SeeOther"
    let [<Literal>] NotModified = prefix + "NotModified"
    let [<Literal>] MovedTemporarily = prefix + "MovedTemporarily"
    let [<Literal>] MultipleRepresentations = prefix + "MultipleRepresentations"

    // 400

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

    // 500

    let [<Literal>] Exception = prefix + "Exception"
    let [<Literal>] NotImplemented = prefix + "NotImplemented"
    let [<Literal>] UnknownMethod = prefix + "UnknownMethod"
    let [<Literal>] ServiceUnavailable = prefix + "ServiceUnavailable"
