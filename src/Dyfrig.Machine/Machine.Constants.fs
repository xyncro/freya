namespace Dyfrig.Machine


[<AutoOpen>]
module Constants =

    [<RequireQualifiedAccess>]
    module internal Actions =

        let [<Literal>] private prefix = "do"

        let [<Literal>] Delete = prefix + "Delete"
        let [<Literal>] Patch = prefix + "Patch"
        let [<Literal>] Post = prefix + "Post"
        let [<Literal>] Put = prefix + "Put"


    [<RequireQualifiedAccess>]
    module internal Configuration =
                
        let [<Literal>] CharsetsAvailable = "charsetsAvailable"
        let [<Literal>] EncodingsAvailable = "encodingsAvailable"
        let [<Literal>] MediaTypesAvailable = "mediaTypesAvailable"
        let [<Literal>] MethodsAllowed = "methodsAllowed"
        let [<Literal>] MethodsKnown = "methodsKnown"
        let [<Literal>] LanguagesAvailable = "languagesAvailable"
        let [<Literal>] ResourceETag = "resourceETag"
        let [<Literal>] ResourceLastModified = "resourceLastModified"


    [<RequireQualifiedAccess>]
    module internal Decisions =

        let [<Literal>] CharsetNegotiable = "charsetNegotiable"
        let [<Literal>] EncodingNegotiable = "encodingNegotiable"
        let [<Literal>] LanguageNegotiable = "languageNegotiable"
        let [<Literal>] MediaTypeNegotiable = "mediaTypeNegotiable"
        let [<Literal>] MethodAllowed = "methodAllowed"
        let [<Literal>] MethodKnown = "methodKnown"
        let [<Literal>] RequestAllowed = "requestAllowed"
        let [<Literal>] RequestAuthorized = "requestAuthorized"
        let [<Literal>] RequestContentTypeKnown = "requestContentTypeKnown"
        let [<Literal>] RequestContentTypeValid = "requestContentTypeValid"
        let [<Literal>] RequestEntityLengthValid = "requestEntityLengthValid"
        let [<Literal>] RequestMalformed = "requestMalformed"
        let [<Literal>] RequestProcessable = "requestProcessable"
        let [<Literal>] RequestUriTooLong = "requestUriTooLong"

        let [<Literal>] ResourceConflicts = "resourceConflicts"
        let [<Literal>] ResourceCreated = "resourceCreated"
        let [<Literal>] ResourceDeleted = "resourceDeleted"
        let [<Literal>] ResourceETagMatchesIf = "resourceETagMatchesIf"
        let [<Literal>] ResourceETagMatchesIfNone = "resourceETagMatchesIfNone"
        let [<Literal>] ResourceExisted = "resourceExisted"
        let [<Literal>] ResourceExists = "resourceExists"
        let [<Literal>] ResourceHasMultipleRepresentations = "resourceHasMultipleRepresentations"
        let [<Literal>] ResourceModifiedSince = "resourceModifiedSince"
        let [<Literal>] ResourceMovedPermanently = "resourceMovedPermanently"
        let [<Literal>] ResourceMovedTemporarily = "resourceMovedTemporarily"
        let [<Literal>] ResourceUnmodifiedSince = "resourceUnmodifiedSince"

        let [<Literal>] ServiceAvailable = "serviceAvailable"

        // TODO - Renaming?

        let [<Literal>] CanPostToGone = "canPostToGone"
        let [<Literal>] CanPostToMissing = "canPostToMissing"
        let [<Literal>] CanPutToMissing = "canPutToMissing"
        let [<Literal>] PostRedirect = "postRedirect"
        let [<Literal>] PutToDifferentUri = "putToDifferentUri"
        let [<Literal>] RespondWithEntity = "respondWithEntity"

        // Internal

        let [<Literal>] RequestAcceptExists = "requestAcceptExists"
        let [<Literal>] RequestAcceptCharsetExists = "requestAcceptCharsetExists"
        let [<Literal>] RequestAcceptEncodingExists = "requestAcceptEncodingExists"
        let [<Literal>] RequestAcceptLanguageExists = "requestAcceptLanguageExists"        
        let [<Literal>] RequestIfMatchExists = "requestIfMatchExists"
        let [<Literal>] RequestIfMatchStar = "requestIfMatchStar"
        let [<Literal>] RequestIfMatchExistsForMissing = "requestIfMatchExistsForMissing"
        let [<Literal>] RequestIfModifiedSinceExists = "requestIfModifiedSinceExists"
        let [<Literal>] RequestIfModifiedSinceValidDate = "requestIfModifiedSinceValidDate"

        // TODO - Renaming?

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


    [<RequireQualifiedAccess>]
    module internal Operations =

        let [<Literal>] private prefix = "pre"

        // 200

        let [<Literal>] PreOK = prefix + "Ok"
        let [<Literal>] PreCreated = prefix + "Created"
        let [<Literal>] PreOptions = prefix + "Options"
        let [<Literal>] PreAccepted = prefix + "Accepted"
        let [<Literal>] PreNoContent = prefix + "NoContent"

        // 300

        let [<Literal>] PreMovedPermanently = prefix + "MovedPermanently"
        let [<Literal>] PreSeeOther = prefix + "SeeOther"
        let [<Literal>] PreNotModified = prefix + "NotModified"
        let [<Literal>] PreMovedTemporarily = prefix + "MovedTemporarily"
        let [<Literal>] PreMultipleRepresentations = prefix + "MultipleRepresentations"

        // 400

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

        // 500

        let [<Literal>] PreException = prefix + "Exception"
        let [<Literal>] PreNotImplemented = prefix + "NotImplemented"
        let [<Literal>] PreUnknownMethod = prefix + "UnknownMethod"
        let [<Literal>] PreServiceUnavailable = prefix + "ServiceUnavailable"
