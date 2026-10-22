port module Main exposing (main)

{-| ECC CRM — Elm frontend entry point.

Routing, authentication flow, data loading, and the update loop live here;
page markup lives in Views.elm.
-}

import Browser
import Char
import Browser.Events
import Browser.Navigation as Nav
import Date
import Html exposing (Html, a, button, div, span, strong, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Http
import Json.Encode as E
import Process
import Task
import Time
import Url
import Url exposing (Url)
import Set exposing (Set)

import Api
import State exposing (..)
import Types
import Ui
import Views
import Features
import Reports
import Dict
import Operations
import Validation
import File
import Json.Decode as D


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- ---------- ports ----------


port saveToken : Maybe String -> Cmd msg
port loadToken : (Maybe String -> msg) -> Sub msg
port focusFeatureForm : () -> Cmd msg
port unsavedChanges : Bool -> Cmd msg
port reportOutput : E.Value -> Cmd msg
port saveReportLayout : E.Value -> Cmd msg
port loadReportLayout : (D.Value -> msg) -> Sub msg
port requestReportLayout : () -> Cmd msg
port saveDark : Bool -> Cmd msg
port loadDark : (Bool -> msg) -> Sub msg



-- ---------- init ----------


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        proto =
            case url.protocol of
                Url.Http ->
                    "http"

                Url.Https ->
                    "https"

        base =
            Api.computeBase { protocol = proto, host = url.host, port_ = url.port_ }
    in
    ( State.initialModel key url |> (\m -> { m | apiBase = base, featureForm = featureDefaultsForUrl url })
    , Cmd.batch
        [ Task.perform ClockTick Time.now
        , Api.clock { base = base, token = Nothing } ClockLoaded
        , if url.path == "/portal" then
            case url.fragment of
                Just fragment -> Task.perform (always (PortalExchange (String.dropLeft 6 fragment))) (Task.succeed ())
                Nothing -> Api.portalGet { base = base, token = Nothing } (acceptLoad (State.initialModel key url) PortalLoaded)
          else Cmd.none
        ]
    )


updateClock : Time.Posix -> Model -> Model
updateClock instant model =
    let
        now = Time.millisToPosix (Time.posixToMillis instant + model.clockSkewMillis)
        zone = Time.customZone model.clockOffsetMinutes []
        pad number = String.padLeft 2 '0' (String.fromInt number)
    in
    { model
        | today = Date.toIsoString (Date.fromPosix zone now)
        , currentTime = String.join ":" [ pad (Time.toHour zone now), pad (Time.toMinute zone now), pad (Time.toSecond zone now) ]
    }


-- Refresh date-based counts at business midnight without reloading editors.
refreshDateSummaries : Model -> Model -> Cmd Msg
refreshDateSummaries previous current =
    if previous.today /= "" && previous.today /= current.today && current.user /= Nothing && current.authChecked then
        fetchDateSummary current
    else
        Cmd.none


fetchDateSummary : Model -> Cmd Msg
fetchDateSummary model =
    if model.user /= Nothing && model.authChecked then
        Api.fetchStats { base = model.apiBase, token = model.token } (acceptLoad model (ClockStatsLoaded model.today))
    else
        Cmd.none


dateSummaryIsStale : Model -> Bool
dateSummaryIsStale model =
    case model.stats of
        Just stats ->
            case D.decodeValue (D.field "date" D.string) stats.today of
                Ok date -> date /= model.today
                Err _ -> False
        Nothing -> False


-- ---------- routing helpers ----------


routeCmd : Model -> Route -> Cmd Msg
routeCmd model route =
    let
        ctx =
            { base = model.apiBase, token = model.token }

        alwaysFetch =
            Cmd.batch
                [ Api.fetchTasksPage ctx model.pageOffset (acceptLoad model TasksLoaded)
                ]
    in
    case route of
        Dashboard ->
            Cmd.batch
                [ Api.fetchStats ctx (acceptLoad model StatsLoaded)
                , Api.fetchLeads ctx (acceptLoad model LeadsLoaded)
                ]

        LeadsPage ->
            Cmd.batch
                [ Api.fetchFilteredLeads ctx model model.pageOffset (acceptLoad model LeadsLoaded)
                , Api.fetchUsers ctx (acceptLoad model UsersLoaded)
                ]

        LeadDetailPage id ->
            Cmd.batch
                [ Api.fetchLeadDetail ctx id (acceptLoad model DetailLoaded)
                , Api.fetchUsers ctx (acceptLoad model UsersLoaded)
                ]

        ClientsPage ->
            Cmd.batch
                [ Api.fetchClientsPage ctx model.pageOffset model.clientQ (acceptLoad model ClientsLoaded)
                , Api.fetchUsers ctx (acceptLoad model UsersLoaded)
                ]

        ClientDetailPage id ->
            Api.fetchClientDetail ctx id (acceptLoad model ClientDetailLoaded)

        TasksPage ->
            Cmd.batch
                [ Api.fetchTasksPage ctx model.pageOffset (acceptLoad model TasksLoaded)
                , Api.fetchUsers ctx (acceptLoad model UsersLoaded)
                , Api.fetchLeads ctx (acceptLoad model LeadsLoaded)
                , Api.fetchClients ctx (acceptLoad model ClientsLoaded)
                ]

        UsersPage ->
            Api.fetchUsers ctx (acceptLoad model UsersLoaded)

        AuditPage ->
            Api.fetchAudit ctx 200 (acceptLoad model AuditLoaded)

        WorkspacePage kind ->
            if kind == "work/applications" then
                Api.fetchApplications ctx model.featureSearch model.applicationFilters model.pageOffset (acceptLoad model FeatureLoaded)
            else if kind == "work/payment-history" then
                Api.financialHistory ctx (Maybe.withDefault "" model.url.query |> String.split "&" |> List.filterMap (\part -> if String.startsWith "invoice=" part then String.dropLeft 8 part |> Url.percentDecode else Nothing) |> List.head) model.featureSearch model.pageOffset (acceptLoad model FeatureLoaded)
            else if String.startsWith "work/" kind then
                Api.featureGet ctx (String.dropLeft 5 kind) model.featureSearch model.pageOffset (acceptLoad model FeatureLoaded)
            else
                Api.fetchOperations ctx kind model.pageOffset (acceptLoad model OperationsLoaded)



-- ---------- update ----------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            if url.path == model.url.path && url.query == model.url.query && (hasDraftChanges model || url.fragment /= model.url.fragment) then
                ( { model | url = url }, Cmd.none )
            else if hasDraftChanges model then
                ( { model | confirmation = Just ("Discard your unsaved changes?", DiscardChanges (NavigateTo (Url.toString url))) }, Nav.replaceUrl model.key (Url.toString model.url) )
            else
                updateNormal msg model
        _ ->
            updateNormal msg model


updateNormal : Msg -> Model -> ( Model, Cmd Msg )
updateNormal msg model =
    let
        guarded =
            case msg of
                LinkClicked (Browser.Internal url) -> url.path /= model.url.path || url.query /= model.url.query
                LinkClicked (Browser.External _) -> True
                NavigateTo _ -> True
                LogoutClicked -> True
                FeatureReset -> True
                FeatureEdit _ -> True
                InlineSection _ -> True
                InlineOwner _ -> True
                CloseTaskEditor -> True
                EditTask _ -> True
                CloseAddLead -> True
                CloseClientForm -> True
                CloseCaseForm -> True
                CloseUserForm -> True
                ToggleGlobalAddTask -> model.showGlobalAddTask
                EditLeadToggled -> model.editLeadOpen
                TogglePwModal -> model.showPwModal
                _ -> False
        busyRequest =
            case msg of
                PortalUpload _ -> model.portalBusy
                PortalRefresh -> model.portalBusy
                FeatureRun _ -> model.featureBusy
                ChecklistUpload _ _ -> model.featureBusy
                FeatureReset -> model.featureBusy
                FeatureEdit _ -> model.featureBusy
                InlineSection _ -> model.featureBusy
                InlineOwner _ -> model.featureBusy
                ApplicationFilter _ _ -> model.featureBusy
                ClearApplicationFilters -> model.featureBusy
                FeatureSearch _ -> model.featureBusy
                PageStep _ -> model.featureBusy || model.documentBusy
                UploadLeadDocClicked _ -> model.documentBusy
                UploadClientDocClicked _ -> model.documentBusy
                ConvertClicked _ -> model.featureBusy
                ClientPhotoUpload _ -> model.clientPhotoBusy
                ClientPhotoRetry _ -> model.clientPhotoBusy
                ClientProfileRefresh _ -> model.clientPhotoBusy
                SubmitAddLead -> model.featureBusy
                SubmitEditLead -> model.featureBusy
                SubmitClientForm -> model.featureBusy
                ContactSaveConfirmed _ _ _ _ -> model.featureBusy
                SubmitAddTaskGlobal -> model.featureBusy
                _ -> False
        recordErrors = recordValidation msg model
        (next, command) =
            if busyRequest then ( model, Cmd.none )
            else if not (Dict.isEmpty recordErrors) then ( { model | featureErrors = recordErrors }, Cmd.none )
            else if guarded && hasDraftChanges model then
                ( { model | confirmation = Just ( "Discard your unsaved changes?", DiscardChanges msg ) }, Cmd.none )
            else
                updateCore msg model
    in
    if hasUnsavedWork next == hasUnsavedWork model then
        ( next, command )
    else
        ( next, Cmd.batch [ command, unsavedChanges (hasUnsavedWork next) ] )


hasUnsavedWork : Model -> Bool
hasUnsavedWork model =
    hasDraftChanges model || model.sessionDraft /= Nothing


hasDraftChanges : Model -> Bool
hasDraftChanges model =
    model.formDirty || model.noteDraft /= "" || model.pendingDoc /= Nothing || model.checklistFile /= Nothing || model.clientPhotoFile /= Nothing || model.financeFile /= Nothing || model.pendingImport /= Nothing


recordValidation : Msg -> Model -> Dict.Dict String String
recordValidation msg model =
    let
        required key label value = if String.trim value == "" then Just (key,label ++ " is required.") else Nothing
        email value =
            if value == "" then Nothing
            else case String.split "@" value of
                [name,domain] -> if name /= "" && domain /= "" && not (String.contains " " value) then Nothing else Just ("email","Enter a valid email address.")
                _ -> Just ("email","Enter a valid email address.")
        contact name address phone =
            [ required "fullName" "Full name" name
            , if String.length (String.trim name)>200 then Just("fullName","Use no more than 200 characters.") else Nothing
            , email (String.trim address)
            , if String.length address>255 then Just("email","Use no more than 255 characters.") else Nothing
            , if String.trim phone == "" then Nothing else
                let digits=String.filter Char.isDigit phone in
                if String.length phone>50 || String.length digits<7 || String.length digits>20 || not (String.all (\c -> Char.isDigit c || String.contains (String.fromChar c) "+()- .") phone) then Just("phone","Enter 7–20 digits; +, spaces, brackets and hyphens are allowed.") else Nothing
            ]
        date key value = if value == "" then Nothing else case Date.fromIsoString value of
            Ok _ -> Nothing
            Err _ -> Just (key,"Enter a valid date.")
        password key value = if Validation.passwordError value == "" then Nothing else Just(key,Validation.passwordError value)
        errors = case msg of
            SubmitAddLead -> contact model.addLead.fullName model.addLead.email model.addLead.phone ++ [date "nextFollowup" model.addLead.nextFollowup]
            SubmitEditLead -> contact model.editLead.fullName model.editLead.email model.editLead.phone ++ [date "lastContact" model.editLead.lastContact,date "nextFollowup" model.editLead.nextFollowup]
            SubmitClientForm -> contact model.clientForm.fullName model.clientForm.email model.clientForm.phone
            SubmitCaseForm _ -> [Maybe.andThen (\f -> date "deadline" f.deadline) model.caseForm]
            SubmitUserForm -> [required "name" "Name" model.userForm.name,required "email" "Email" model.userForm.email,email model.userForm.email,if model.userForm.password == "" then Nothing else password "password" model.userForm.password]
            SubmitPasswordChange -> [required "pwCurrent" "Current password" model.pwCurrent,password "pwNew" model.pwNew,if model.pwNew /= model.pwConfirm then Just("pwConfirm","Passwords do not match.") else Nothing]
            SubmitAddTaskFor _ -> [required "title" "Task title" model.addTask.title]
            SubmitClientTask _ -> [required "title" "Task title" model.addTask.title]
            _ -> []
    in
    List.filterMap identity errors |> Dict.fromList


draftFields : Dict.Dict String String -> Dict.Dict String String
draftFields =
    Dict.filter (\key _ -> not (List.member (String.toLower key) ["password","code","secret","currentpassword","newpassword","pwcurrent","pwnew","pwconfirm","token","url"]))


captureSessionDraft : Model -> Maybe SessionDraft
captureSessionDraft model =
    if model.user == Nothing || not (hasDraftChanges model) then
        model.sessionDraft
    else
        Maybe.map (\user ->
            let safeFeature = List.member (featureSection model) ["tasks","history","documents","applications","invoices","payments","payment-history","expenses","periods","reconciliation","appointments"] in
            { path = Url.toString model.url
            , ownerId = user.id
            , addLead = model.addLead
            , editLead = model.editLead
            , addTask = model.addTask
            , clientForm = model.clientForm
            , caseForm = model.caseForm
            , noteDraft = model.noteDraft
            , featureForm = if safeFeature then draftFields model.featureForm else Dict.empty
            , featureBaseline = if safeFeature then draftFields model.featureBaseline else Dict.empty
            , recordBaseline = if featureSection model == "security" then E.null else model.recordBaseline
            , inlineSection = model.inlineSection
            , inlineOwner = model.inlineOwner
            , formDirty = model.formDirty
            , editingTask = model.editingTask
            , showAddLead = model.showAddLead
            , editLeadOpen = model.editLeadOpen
            , showGlobalAddTask = model.showGlobalAddTask
            , showClientForm = model.showClientForm
            , clientEditingId = model.clientEditingId
            }
        ) model.user


clearDraftInputs : Model -> Model
clearDraftInputs model =
    let empty = State.initialModel model.key model.url in
    { model
        | addLead = empty.addLead
        , editLead = empty.editLead
        , addTask = empty.addTask
        , clientForm = empty.clientForm
        , caseForm = empty.caseForm
        , noteDraft = empty.noteDraft
        , featureForm = empty.featureForm
        , featureBaseline = empty.featureBaseline
        , recordBaseline = empty.recordBaseline
        , inlineSection = empty.inlineSection
        , inlineOwner = empty.inlineOwner
        , formDirty = empty.formDirty
        , editingTask = empty.editingTask
        , showAddLead = empty.showAddLead
        , editLeadOpen = empty.editLeadOpen
        , showGlobalAddTask = empty.showGlobalAddTask
        , showClientForm = empty.showClientForm
        , clientEditingId = empty.clientEditingId
        , showUserForm = empty.showUserForm
        , userForm = empty.userForm
        , showPwModal = empty.showPwModal
        , pwCurrent = empty.pwCurrent
        , pwNew = empty.pwNew
        , pwConfirm = empty.pwConfirm
        , pendingDoc = empty.pendingDoc
        , pendingImport = empty.pendingImport
        , clientPhotoPreview = empty.clientPhotoPreview
        , clientPhotoFile = empty.clientPhotoFile
        , checklistFile = empty.checklistFile
        , financeFile = empty.financeFile
        , conflictCurrent = empty.conflictCurrent
        , conflictOriginal = empty.conflictOriginal
        , conflictEdits = empty.conflictEdits
        , confirmation = Nothing
    }


restoreSessionDraft : Int -> Model -> Model
restoreSessionDraft uid model =
    case model.sessionDraft of
        Just draft ->
            if draft.ownerId == uid && draft.path == Url.toString model.url then
                { model
                    | addLead = draft.addLead
                    , editLead = draft.editLead
                    , addTask = draft.addTask
                    , clientForm = draft.clientForm
                    , caseForm = draft.caseForm
                    , noteDraft = draft.noteDraft
                    , featureForm = draft.featureForm
                    , featureBaseline = draft.featureBaseline
                    , recordBaseline = draft.recordBaseline
                    , inlineSection = draft.inlineSection
                    , inlineOwner = draft.inlineOwner
                    , formDirty = draft.formDirty
                    , editingTask = draft.editingTask
                    , showAddLead = draft.showAddLead
                    , editLeadOpen = draft.editLeadOpen
                    , showGlobalAddTask = draft.showGlobalAddTask
                    , showClientForm = draft.showClientForm
                    , clientEditingId = draft.clientEditingId
                    , sessionDraft = Nothing
                }
            else
                { model | sessionDraft = Nothing }
        Nothing ->
            model


updateCore : Msg -> Model -> ( Model, Cmd Msg )
updateCore msg model =
    case msg of
        AcceptLoad key loaded ->
            if key == loadKeyFor loaded model then
                update loaded model
            else
                ( model, Cmd.none )

        _ ->
            if expiredSession msg then
                let
                    draft = captureSessionDraft model
                    cleared = clearDraftInputs model
                in
                ( { cleared | sessionDraft = draft, user = Nothing, token = Nothing, authChecked = True, clientProfile = E.null, clientPhotoFile = Nothing, clientPhotoPreview = Nothing, clientPhotoBusy = False, clientPhotoNotice = "", clientPhotoError = "", formDirty = False, editingTask = False, portalBusy = False, documentBusy = False, photoEpoch = model.photoEpoch + 1, loadEpoch = model.loadEpoch + 1, userEpoch = model.userEpoch + 1, loginError = "Your session has expired. Please sign in again.", loginNotice = if draft /= Nothing then "Your unsaved text is held in this tab. Sign in to the same account to recover it. Re-select any attachments." else "", loginPassword = "", loginOtp = "", featureData = E.null, financeCandidates = [], financeFile = Nothing, financeCandidateQuery = "", financeCandidateOffset = 0, featureResult = E.null, reportSettings = Dict.empty, featureForm = Dict.empty, portalData = E.null, portalFile = Nothing, conflictCurrent = E.null, featureBaseline = Dict.empty, conflictOriginal = Dict.empty, conflictEdits = Dict.empty, recordBaseline = E.null, checklistFile = Nothing, featureBusy = False, detail = Nothing, clientDetail = Nothing, leads = [], clients = [], tasks = [], users = [], opsData = [], audit = [] }, saveToken Nothing )
            else
                updateHelp msg { model | photoEpoch = model.photoEpoch + (if startsPhotoLoad msg then 1 else 0), loadEpoch = model.loadEpoch + (if startsLoad msg then 1 else 0), userEpoch = model.userEpoch + (if startsUserLoad msg then 1 else 0) }


acceptLoad : Model -> (a -> Msg) -> a -> Msg
acceptLoad model constructor result =
    let loaded = constructor result in
    AcceptLoad (loadKeyFor loaded model) loaded


loadKeyFor : Msg -> Model -> String
loadKeyFor msg model =
    case msg of
        ClientProfileLoaded _ -> photoLoadKey model
        ClientPhotoDone _ _ -> photoLoadKey model
        UsersLoaded _ -> E.encode 0 (E.list E.string [ Maybe.withDefault "" model.token, String.fromInt model.userEpoch ])
        _ -> loadKey model



-- Photo requests belong to the client, not its currently selected workspace panel.
-- Keep a separate generation so panel searches cannot strand an upload, while
-- navigation, sign-out and newer photo requests still reject stale responses.
photoLoadKey : Model -> String
photoLoadKey model =
    E.encode 0 (E.list E.string
        [ model.url.path, Maybe.withDefault "" model.token, String.fromInt model.photoEpoch ])


startsPhotoLoad : Msg -> Bool
startsPhotoLoad msg =
    case msg of
        ClientDetailLoaded (Ok _) -> True
        ClientProfileRefresh _ -> True
        ClientPhotoUpload _ -> True
        ClientPhotoRetry _ -> True
        ClientPhotoDone _ (Ok _) -> True
        UrlChanged _ -> True
        LoginDone (Ok _) -> True
        LogoutDone (Ok _) -> True
        _ -> False


startsUserLoad : Msg -> Bool
startsUserLoad msg =
    case msg of
        UserSaved (Ok _) -> True
        UserUpdatedDone (Ok _) -> True
        DeleteUserDone (Ok _) -> True
        LogoutClicked -> True
        LogoutDone (Ok _) -> True
        PasswordChangeDone (Ok _) -> True
        UrlChanged _ -> True
        _ -> False


loadKey : Model -> String
loadKey model =
    E.encode 0 (E.list E.string
        [ model.url.path, Maybe.withDefault "" model.token, String.fromInt model.loadEpoch, String.fromInt model.pageOffset
        , model.q, model.clientQ, model.fStage, model.fQuality, model.fSource
        , E.encode 0 (E.object (Dict.toList model.applicationFilters |> List.map (Tuple.mapSecond E.string)))
        , model.sortBy, model.inlineSection, model.inlineOwner, model.featureSearch, if model.sortAsc then "asc" else "desc"
        ])


startsLoad : Msg -> Bool
startsLoad msg =
    case msg of
        ConvertClicked _ -> True
        EditTask _ -> True
        CloseTaskEditor -> True
        UrlChanged _ -> True
        PageStep _ -> True
        SearchReady _ _ -> True
        FeatureEdit _ -> True
        FeatureReset -> True
        InlineSection _ -> True
        InlineOwner _ -> True
        ApplicationFilter _ _ -> True
        ClearApplicationFilters -> True
        FeatureSearch _ -> True
        PortalExchange _ -> True
        PortalUpload _ -> True
        PortalRefresh -> True
        PortalLogout -> True
        PortalLogoutDone (Ok _) -> True
        PortalUploaded (Ok _) -> True
        FeatureDone (Ok _) -> True
        SearchChanged _ -> True
        ClientSearchChanged _ -> True
        StageFilterChanged _ -> True
        QualityFilterChanged _ -> True
        SourceFilterChanged _ -> True
        SortByChanged _ -> True
        SortDirToggled -> True
        ClearFilters -> True
        LogoutClicked -> True
        LogoutDone (Ok _) -> True
        PasswordChangeDone (Ok _) -> True
        NavigateTo _ -> True
        OperationDone (Ok _) -> True
        AddLeadDone (Ok _) -> True
        EditLeadDone (Ok _) -> True
        DeleteLeadDone (Ok _) -> True
        AdvanceDone (Ok _) -> True
        ConvertDone (Ok _) -> True
        NoteDone (Ok _) -> True
        BulkDone (Ok _) -> True
        TaskStatusDone (Ok _) -> True
        TaskDeleteDone (Ok _) -> True
        AddTaskDone (Ok _) -> True
        ClientUpdated (Ok _) -> True
        ClientFormSaved (Ok _) -> True
        DeleteClientDone (Ok _) -> True
        CaseFormSaved (Ok _) -> True
        DeleteCaseDone (Ok _) -> True
        LeadDocUploaded (Ok _) -> True
        ClientDocUploaded (Ok _) -> True
        DocDeleteDone (Ok _) -> True
        LeadsImported (Ok _) -> True
        UserSaved (Ok _) -> True
        UserUpdatedDone (Ok _) -> True
        DeleteUserDone (Ok _) -> True
        _ -> False


updateHelp : Msg -> Model -> ( Model, Cmd Msg )
updateHelp msg model =
    let
        ctx =
            { base = model.apiBase, token = model.token }
    in
    case msg of
        ContactCheckDone target signature payload result ->
            if signature /= contactSignature target model || not (contactFormOpen target model) then
                ( { model | featureBusy = False }, Cmd.none )
            else case result of
                Err err -> ( { model | featureBusy = False }, toast "error" ("Could not check for duplicates. Your entries are kept. Retry Save. " ++ httpError err) )
                Ok response ->
                    case D.decodeValue (D.field "matches" (D.list D.value)) response of
                        Err _ -> ( { model | featureBusy = False }, toast "error" "Could not check for duplicates. Your entries are kept. Retry Save." )
                        Ok matches ->
                            if List.isEmpty matches then updateCore (ContactSaveConfirmed target signature payload []) { model | featureBusy = False }
                            else ( { model | featureBusy = False, confirmation = Just ("Another record uses this email or phone. Review the matches before creating a separate record. Shared family contact details can be valid.", ContactSaveConfirmed target signature payload matches) }, Cmd.none )

        ContactSaveConfirmed target signature payload _ ->
            if signature /= contactSignature target model || not (contactFormOpen target model) then
                ( model, toast "error" "Details changed after the check. Review them and select Save again." )
            else
                let saving = { model | featureBusy = True } in
                case target of
                    "add-lead" -> ( saving, Api.createLead ctx payload AddLeadDone )
                    "edit-lead" -> case model.detail of
                        Just detail -> ( saving, Api.updateLead ctx detail.lead.id payload EditLeadDone )
                        Nothing -> ( model, Cmd.none )
                    "student" -> case model.clientEditingId of
                        Just cid -> ( saving, Api.updateClient ctx cid payload ClientUpdated )
                        Nothing -> ( saving, Api.createClient ctx payload ClientFormSaved )
                    _ -> ( model, Cmd.none )

        ConfirmRequested message action ->
            ( { model | confirmation = Just ( message, action ) }, Cmd.none )

        ConfirmDismissed ->
            ( { model | confirmation = Nothing }, Cmd.none )

        ConfirmAccepted ->
            case model.confirmation of
                Just ( _, action ) -> update action { model | confirmation = Nothing }
                Nothing -> ( model, Cmd.none )

        AcceptLoad _ _ ->
            ( model, Cmd.none )

        LoginOtpChanged value ->
            ( { model | loginOtp = value }, Cmd.none )

        FeatureLoaded result ->
            case result of
                Ok value ->
                    let pref = D.decodeValue (D.field "security" D.value) value |> Result.withDefault E.null |> Features.rowForm |> Dict.map (\key v -> if List.member key ["assignments","deadlines","documents","escalations","emailEnabled"] then (if v=="1" || v=="true" then "true" else "false") else v) in
                    ( { model | featureForm = if featureSection model=="notifications" && not model.formDirty then Dict.union pref model.featureForm else if model.inlineOwner/="" && Dict.get "owner" model.featureForm==Nothing then Dict.insert "owner" model.inlineOwner model.featureForm else if featureSection model=="expenses" && (Dict.get "submissionKey" model.featureForm |> Maybe.withDefault "")=="" then Dict.insert "submissionKey" (Features.str "submissionKey" value) model.featureForm else model.featureForm, featureData = value, opsData = D.decodeValue (D.field "rows" (D.list D.value)) value |> Result.withDefault [], pageLoading = False, pageError = "" }, Cmd.none )
                Err err -> ( { model | pageLoading = False, pageError = httpError err }, Cmd.none )

        InlineSection section ->
            let
                own = if section == "applications" then Maybe.map (\detail -> "client:" ++ String.fromInt detail.client.id) model.clientDetail |> Maybe.withDefault model.inlineOwner else model.inlineOwner
                next = { model | inlineSection = section, inlineOwner = own, featureForm = Dict.insert "owner" own (Features.defaults section), featureSearch = "", featureResult = E.null, featureError = "", featureErrors = Dict.empty, pageOffset = 0, featureBusy = False, checklistFile = Nothing }
            in
            ( next, reloadPanel next )

        InlineOwner own ->
            let next = { model | inlineOwner = own, featureForm = Dict.insert "owner" own (Features.defaults model.inlineSection), featureSearch = "", pageOffset = 0, featureResult = E.null, featureError = "", featureBusy = False } in
            ( next, reloadPanel next )

        ChecklistFile file ->
            if File.size file <= 0 || File.size file > 25 * 1024 * 1024 then
                ( { model | checklistFile = Nothing, featureError = "Choose a nonempty file up to 25 MB." }, Cmd.none )
            else
                ( { model | checklistFile = Just file, formDirty = True }, Cmd.none )

        ChecklistUpload rid revision ->
            case model.checklistFile of
                Just file ->
                    if model.featureBusy then ( model, Cmd.none ) else
                    ( { model | featureBusy = True, featureError = "", featureForm = Dict.insert "id" (String.fromInt rid) model.featureForm }, Api.checklistUpload ctx rid revision file (acceptLoad model FeatureDone) )
                Nothing -> ( { model | featureError = "Choose a file to upload." }, Cmd.none )

        ConflictLoaded result ->
            case result of
                Ok current -> ( { model | conflictCurrent = current }, Cmd.none )
                Err err -> ( { model | featureError = httpError err }, toast "error" (httpError err) )

        ConflictDismiss -> ( { model | conflictCurrent = E.null }, Cmd.none )

        ConflictUseCurrent key ->
            let latest = Features.formValues model.conflictCurrent |> Dict.get key |> Maybe.withDefault "" in
            ( { model | featureForm = Dict.insert key latest model.featureForm, conflictEdits = Dict.insert key latest model.conflictEdits }, Cmd.none )

        ConflictRebase ->
            let
                revision = D.decodeValue (D.field "revision" D.int) model.conflictCurrent |> Result.withDefault 0
                cf = model.clientForm
                ef = model.editLead
                uf = model.userForm
            in
            ( { model | conflictCurrent = E.null, recordBaseline = model.conflictCurrent, featureForm = Dict.insert "expectedRevision" (String.fromInt revision) model.featureForm, featureBaseline = Features.formValues model.conflictCurrent, clientForm = { cf | revision = if model.conflictKind == "clients" then revision else cf.revision }, editLead = { ef | revision = if model.conflictKind == "leads" then revision else ef.revision }, userForm = { uf | revision = if model.conflictKind == "users" then revision else uf.revision }, caseForm = Maybe.map (\f -> { f | revision = if model.conflictKind == "cases" then revision else f.revision }) model.caseForm, featureError = "Comparison accepted. Review your form and save when ready." }, Cmd.none )

        ApplicationFilter key value ->
            let next = { model | applicationFilters = Dict.insert key value model.applicationFilters, pageOffset = 0, pageLoading = True } in
            (next,reloadPanel next)

        ClearApplicationFilters ->
            let next = { model | applicationFilters = Dict.empty, featureSearch = "", pageOffset = 0, pageLoading = True } in
            (next,reloadPanel next)

        ReportSetting key value ->
            ( { model | reportSettings = Dict.insert key value model.reportSettings }, Cmd.none )

        ReportSettingsLoaded value ->
            ( { model | reportSettings = D.decodeValue (D.dict D.string) value |> Result.withDefault Dict.empty }, Cmd.none )

        ReportOutput action ->
            if action == "save-template" then
                ( model, saveReportLayout (E.dict identity E.string model.reportSettings) )
            else if action == "load-template" then
                ( model, requestReportLayout () )
            else if model.pageLoading || model.pageError /= "" then
                ( model, Cmd.none )
            else
                ( model, reportOutput (Reports.payload model (featureSection model) action) )

        FeatureField key value ->
            ( { model | featureForm = Dict.insert key value model.featureForm, featureErrors = Dict.remove key model.featureErrors, formDirty = True }, Cmd.none )

        FeatureSearch query ->
            let next = { model | featureSearch = query, pageOffset = 0, pageLoading = True } in
            ( next, reloadPanel next )

        FeatureEdit row ->
            let
                next = { model | featureForm = Dict.union (Features.rowForm row) (Dict.insert "owner" model.inlineOwner (Features.defaults (featureSection model))), featureBaseline = Features.formValues row, conflictCurrent = E.null, featureBusy = False, featureResult = E.null, featureError = "", featureErrors = Dict.empty, formDirty = False, financeFile = Nothing, financeCandidates = [], financeCandidateQuery = "", financeCandidateOffset = 0 }
            in
            ( next, Cmd.batch [focusFeatureForm (), if featureSection model == "reconciliation" then loadFinanceCandidates next else Cmd.none] )

        FinanceFile file ->
            if File.size file <= 0 || File.size file > 10 * 1024 * 1024 then
                ( { model | financeFile = Nothing, featureError = "Choose a nonempty PDF, PNG or JPEG up to 10 MB." }, Cmd.none )
            else ( { model | financeFile = Just file }, Cmd.none )

        FinanceUpload eid revision ->
            case model.financeFile of
                Just file -> ( { model | featureBusy = True, featureError = "" }, Api.financeAttachment ctx eid revision file (acceptLoad model FeatureDone) )
                Nothing -> ( { model | featureError = "Choose a supporting file first." }, Cmd.none )

        FinanceCandidatesLoaded tag result ->
            if tag /= financeCandidateTag model then (model,Cmd.none) else
            case result of
                Ok value -> ( { model | financeCandidates = D.decodeValue (D.field "rows" (D.list D.value)) value |> Result.withDefault [] }, Cmd.none )
                Err err -> ( { model | featureError = httpError err }, Cmd.none )

        FinanceCandidateQuery query ->
            let next = { model | financeCandidateQuery = query, financeCandidateOffset = 0 } in
            (next,loadFinanceCandidates next)

        FinanceCandidateStep direction ->
            let next = { model | financeCandidateOffset = max 0 (model.financeCandidateOffset + direction * 50) } in
            (next,loadFinanceCandidates next)

        FinanceCandidateSelected value ->
            ( { model | featureForm = model.featureForm |> Dict.insert "match" (Features.str "value" value) |> Dict.insert "matchLabel" (Features.str "label" value), formDirty = True }, Cmd.none )

        DiscardChanges action ->
            update action { model | formDirty = False, confirmation = Nothing, noteDraft = "", pendingDoc = Nothing, checklistFile = Nothing, clientPhotoFile = Nothing, clientPhotoPreview = Nothing, financeFile = Nothing, pendingImport = Nothing, sessionDraft = Nothing }

        EditTask task ->
            let
                next = { model | editingTask = True, featureErrors = Dict.empty, featureError = "", featureBusy = True, featureResult = E.null, featureForm = Dict.fromList
                    [ ("id", String.fromInt task.id), ("expectedRevision", String.fromInt task.revision)
                    , ("title", task.title), ("description", Maybe.withDefault "" task.description)
                    , ("dueDate", Maybe.withDefault "" task.dueDate), ("priority", task.priority), ("status", task.status)
                    , ("assignee", Maybe.map String.fromInt task.assignedTo |> Maybe.withDefault "")
                    , ("recurrenceDays", "")
                    ] }
            in
            ( next, Api.featureGet ctx ("conflict/tasks/" ++ String.fromInt task.id) "" 0 (acceptLoad next TaskEditorLoaded) )

        TaskEditorLoaded result ->
            case result of
                Ok row ->
                    ( { model | featureBusy = False, featureForm = Features.rowForm row, featureBaseline = Features.formValues row }, Cmd.none )
                Err err ->
                    ( { model | featureBusy = False, editingTask = False }, toast "error" (httpError err) )

        CloseTaskEditor ->
            ( { model | editingTask = False, formDirty = False, featureErrors = Dict.empty, featureError = "" }, Cmd.none )

        FeatureReset ->
            let next = { model | featureForm = Dict.insert "owner" model.inlineOwner (Dict.insert "submissionKey" (Features.str "submissionKey" model.featureData) (Features.defaults (featureSection model))), featureResult = E.null, featureError = "", featureBusy = False, conflictCurrent = E.null, featureErrors = Dict.empty, formDirty = False, financeFile = Nothing, financeCandidates = [], financeCandidateQuery = "", financeCandidateOffset = 0 } in
            ( next, reloadCurrent next )

        FeatureRun action ->
            let errors = Validation.feature (featureSection model) action model.inlineOwner model.featureForm in
            if model.featureBusy then ( model, Cmd.none )
            else if not (Dict.isEmpty errors) then
                ( { model | featureErrors = errors, featureError = Dict.values errors |> List.head |> Maybe.withDefault "Check the highlighted fields before continuing." }, Cmd.none )
            else if String.startsWith "rescan-file:" action then
                ( { model | featureBusy = True }, Api.rescanFinancialFile ctx (String.dropLeft 12 action) (acceptLoad model FeatureDone) )
            else if action == "apply" && featureSection model == "search" then
                let next = { model | featureSearch = "@view:" ++ Maybe.withDefault "" (Dict.get "id" model.featureForm), pageOffset = 0, pageLoading = True, featureResult = E.null } in
                ( next, reloadCurrent next )
            else if action == "range" then
                let next = { model | featureSearch = if Maybe.withDefault "" (Dict.get "from" model.featureForm) == "" && Maybe.withDefault "" (Dict.get "to" model.featureForm) == "" then "" else Maybe.withDefault "" (Dict.get "from" model.featureForm) ++ "," ++ Maybe.withDefault "" (Dict.get "to" model.featureForm), pageLoading = True, formDirty = False } in
                ( next, reloadCurrent next )
            else
                ( { model | featureBusy = True, featureError = "" }, Api.featurePost ctx ((if featureSection model == "payment-history" then "payments" else featureSection model) ++ "/" ++ action) (E.object (Dict.toList (if model.inlineOwner/="" && (Dict.get "owner" model.featureForm |> Maybe.withDefault "")=="" then Dict.insert "owner" model.inlineOwner model.featureForm else model.featureForm) |> List.map (Tuple.mapSecond E.string))) (acceptLoad model FeatureDone) )

        FeatureDone result ->
            case result of
                Ok value ->
                    let
                        addRevision key fields =
                            case D.decodeValue (D.field key D.int) value of
                                Ok n -> Dict.insert key (String.fromInt n) fields
                                Err _ -> fields
                        form =
                            if List.member (featureSection model) ["merge","security","notifications","search"] then
                                List.foldl addRevision model.featureForm ["sourceRevision","targetRevision"]
                            else
                                Dict.insert "owner" model.inlineOwner (Features.defaults (featureSection model))
                    in
                    ( { model | featureBusy = False, featureError = "", featureErrors = Dict.empty, formDirty = False, editingTask = False, showGlobalAddTask = False, addTask = State.emptyAddTask, checklistFile = Nothing, financeFile = Nothing, featureResult = value, featureForm = Dict.remove "password" (Dict.remove "code" form) }, reloadCurrent model )
                Err (Http.BadStatus 409) ->
                    if not (List.member (featureSection model) ["checklist-templates","applications","milestones","tasks","documents","reminders","appointments","invoices","notifications"]) then
                        ( { model | featureBusy = False, featureError = if List.member (featureSection model) ["payments","expenses","payment-history","periods","reconciliation"] then "This financial record changed or the entry was already submitted. Reload the list and select the current record before trying again. Your form has been kept." else "This action is unavailable in its current state. Refresh or preview the latest records. Email retries require an exhausted failure, no active send, and fewer than three manual retries spaced 15 minutes apart." }, Cmd.none )
                    else
                    let rid = Dict.get (if featureSection model=="reminders" then "task" else "id") model.featureForm |> Maybe.andThen String.toInt |> Maybe.withDefault 0 in
                    beginConflict { model | featureBusy = False, featureError = "The item changed. Compare the current version below." } (featureSection model) rid (dictValue model.featureBaseline) (dictValue model.featureForm)
                Err err -> ( { model | featureBusy = False, featureError = httpError err }, toast "error" (httpError err) )

        PortalExchange token ->
            ( { model | portalBusy = True, portalError = "" }, Api.portalPost ctx "exchange" (E.object [("token",E.string token)]) PortalExchanged )

        PortalExchanged result ->
            case result of
                Ok _ -> ( { model | portalBusy = False, portalError = "" }, Nav.replaceUrl model.key "/portal" )
                Err err -> ( { model | portalBusy = False, portalError = "This sign-in link is invalid, expired, or already used. Ask your case officer for a new link." }, Nav.replaceUrl model.key "/portal" )

        PortalLoaded result ->
            case result of
                Ok value -> ( { model | portalData = value, portalBusy = False, portalError = "" }, Cmd.none )
                Err err -> ( { model | portalBusy = False, portalError = if model.portalError /= "" then model.portalError else "Your student access has expired or been revoked. Ask your case officer for a new sign-in link.", portalData = E.null }, Cmd.none )

        PortalFile file ->
            if File.size file <= 0 || File.size file > 25 * 1024 * 1024 then
                ( { model | portalFile = Nothing, portalError = "Choose a nonempty file up to 25 MB." }, Cmd.none )
            else
                ( { model | portalFile = Just file, portalError = "", portalNotice = "" }, Cmd.none )

        PortalUpload rid ->
            case model.portalFile of
                Just file -> if model.portalBusy then ( model, Cmd.none ) else ( { model | portalBusy = True, portalError = "", portalNotice = "" }, Api.portalUpload ctx rid file (acceptLoad model PortalUploaded) )
                Nothing -> ( model, Cmd.none )

        PortalUploaded result ->
            case result of
                Ok _ -> ( { model | portalBusy = False, portalFile = Nothing, portalNotice = "File received. It will be scanned before your case officer reviews it." }, Api.portalGet ctx (acceptLoad model PortalLoaded) )
                Err err -> ( { model | portalBusy = False, portalError = httpError err }, Cmd.none )

        PortalRefresh ->
            if model.portalBusy then ( model, Cmd.none ) else
            ( { model | portalBusy = True }, Api.portalGet ctx (acceptLoad model PortalLoaded) )

        PortalLogout ->
            ( { model | portalBusy = True }, Api.portalPost ctx "logout" (E.object []) PortalLogoutDone )

        PortalLogoutDone result ->
            case result of
                Ok _ -> ( { model | portalData = E.null, portalFile = Nothing, portalBusy = False, portalNotice = "", portalError = "Signed out. Ask your case officer for a new link when you need access again." }, Cmd.none )
                Err err -> ( { model | portalBusy = False, portalError = "Sign-out failed. Please try again. " ++ httpError err }, Cmd.none )

        LoginModeChanged mode ->
            ( { model | loginMode = mode, loginError = "", loginNotice = "", loginPassword = "" }, Cmd.none )

        RecoverClicked ->
            if model.loginBusy then ( model, Cmd.none ) else if not (String.contains "@" model.loginEmail) then
                ( { model | loginError = "Enter your account email address." }, Cmd.none )
            else ( { model | loginBusy = True, loginError = "" }, Api.recover ctx model.loginEmail RecoveryDone )

        ResetClicked ->
            if model.loginBusy then ( model, Cmd.none ) else if Validation.passwordError model.loginPassword /= "" then
                ( { model | loginError = Validation.passwordError model.loginPassword }, Cmd.none )
            else let resetToken = Maybe.withDefault "" model.url.query |> String.dropLeft 6 |> Url.percentDecode |> Maybe.withDefault "" in
            ( { model | loginBusy = True, loginError = "" }, Api.resetPassword ctx resetToken model.loginPassword ResetDone )

        RecoveryDone result ->
            case result of
                Ok _ -> ( { model | loginBusy = False, loginNotice = "If your account exists, you’ll receive an email with a link that expires in one hour." }, Cmd.none )
                Err err -> ( { model | loginBusy = False, loginError = httpError err }, Cmd.none )

        ResetDone result ->
            case result of
                Ok _ -> ( { model | loginBusy = False, loginMode = "login", loginPassword = "", loginNotice = "Password updated. Sign in with your new password." }, Nav.replaceUrl model.key "/" )
                Err err -> ( { model | loginBusy = False, loginError = httpError err }, Cmd.none )

        LogoutDone result ->
            let cleared = clearDraftInputs model in
            case result of
                Ok _ -> ( { cleared | sessionDraft = Nothing, user = Nothing, token = Nothing, authChecked = True, clientProfile = E.null, clientPhotoFile = Nothing, clientPhotoPreview = Nothing, clientPhotoBusy = False, clientPhotoNotice = "", clientPhotoError = "", formDirty = False, editingTask = False, portalBusy = False, documentBusy = False, loginPassword = "", loginOtp = "", featureData = E.null, financeCandidates = [], financeFile = Nothing, financeCandidateQuery = "", financeCandidateOffset = 0, featureResult = E.null, reportSettings = Dict.empty, featureForm = Dict.empty, portalData = E.null, portalFile = Nothing, conflictCurrent = E.null, featureBaseline = Dict.empty, conflictOriginal = Dict.empty, conflictEdits = Dict.empty, recordBaseline = E.null, checklistFile = Nothing, featureBusy = False, detail = Nothing, clientDetail = Nothing, leads = [], clients = [], tasks = [], users = [], audit = [], opsData = [] }, Cmd.batch [ saveToken Nothing, Nav.pushUrl model.key "/" ] )
                Err err -> ( model, toast "error" (httpError err) )

        PageStep step ->
            let next = { model | pageOffset = max 0 (model.pageOffset + step * 50), pageLoading = True, selected = Set.empty } in
            ( next, reloadPanel next )

        SearchReady kind query ->
            if (kind == "leads" && query == model.q) || (kind == "clients" && query == model.clientQ) then
                ( { model | pageLoading = True }, if kind == "leads" then Api.fetchFilteredLeads ctx model 0 (acceptLoad model LeadsLoaded) else Api.fetchClientsPage ctx 0 query (acceptLoad model ClientsLoaded) )
            else
                ( model, Cmd.none )

        OperationsLoaded result ->
            ( { model | opsData = Result.withDefault [] result, pageLoading = False, pageError = resultError result }, Cmd.none )

        RunOperation path ->
            ( model, Api.operation ctx path OperationDone )

        OperationDone result ->
            case result of
                Ok _ -> ( model, Cmd.batch [ toast "success" "Saved successfully", reloadCurrent model ] )
                Err err -> ( model, toast "error" (httpError err) )

        NoOp ->
            ( model, Cmd.none )

        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    if url.path == model.url.path && url.query == model.url.query then
                        (model,Nav.pushUrl model.key (Url.toString url))
                    else
                        update (NavigateTo (Url.toString url)) model

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                next =
                    { model
                        | url = url
                        , formDirty = False
                        , editingTask = False
                        , clientProfile = E.null
                        , clientPhotoFile = Nothing, clientPhotoPreview = Nothing
                        , clientPhotoBusy = False
                        , clientPhotoNotice = ""
                        , clientPhotoError = ""
                        , featureErrors = Dict.empty
                        , applicationFilters = State.applicationFiltersForUrl url
                        , featureSearch = ""
                        , featureForm = featureDefaultsForUrl url
                        , featureResult = E.null
                        , pageOffset = 0
                        , selected = Set.empty
                        , pageLoading = True
                        , pageError = ""
                        , opsData = []
                        , inlineOwner = ownerForUrl url
                        , featureBaseline = Dict.empty
                        , conflictCurrent = E.null
                        , checklistFile = Nothing
                        , featureBusy = False
                        , featureError = ""
                        , notifOpen = False
                        , detail = Nothing
                        , clientDetail = Nothing
                        , pendingDoc = Nothing
                        , pendingImport = Nothing
                    }
            in
            ( next
            , if url.path == "/portal" then
                case url.fragment of
                    Just fragment -> Task.perform (always (PortalExchange (String.dropLeft 6 fragment))) (Task.succeed ())
                    Nothing -> Api.portalGet ctx (acceptLoad next PortalLoaded)

              else if model.user == Nothing then
                Cmd.none

              else
                routeCmd next (State.routeOf url)
            )

        GotStoredToken t ->
            if t == Nothing then
                ( { model | authChecked = True }, Cmd.none )

            else
                ( { model | token = t }, Api.me { base = model.apiBase, token = t } MeDone )

        GotStoredDark flag ->
            ( { model | darkMode = flag }, Cmd.none )

        ToggleDarkMode ->
            let
                newFlag =
                    not model.darkMode
            in
            ( { model | darkMode = newFlag }, saveDark newFlag )

        TodayChanged iso ->
            ( { model | today = iso }, Cmd.none )

        ClockTick instant ->
            let next = updateClock instant model in
            ( next, refreshDateSummaries model next )

        ClockRefresh instant ->
            let
                next = updateClock instant model
                summary = if next.today == model.today && dateSummaryIsStale next then fetchDateSummary next else refreshDateSummaries model next
            in
            ( next, Cmd.batch [ Api.clock ctx ClockLoaded, summary ] )

        ClockRefreshRequested ->
            ( model, Cmd.batch [ Task.perform ClockTick Time.now, Api.clock ctx ClockLoaded ] )

        ClockLoaded result ->
            case result |> Result.andThen (D.decodeValue (D.map3 (\millis offset zone -> ( millis, offset, zone )) (D.field "unixMillis" D.int) (D.field "offsetMinutes" D.int) (D.field "timezone" D.string)) >> Result.mapError (always Http.NetworkError)) of
                Ok ( millis, offset, zone ) ->
                    if offset >= -720 && offset <= 840 then
                        ( model, Task.perform (ClockAligned millis offset zone) Time.now )
                    else
                        ( { model | clockSynced = False }, Cmd.none )
                Err _ ->
                    ( { model | clockSynced = False }, Cmd.none )

        ClockAligned millis offset zone received ->
            let
                synced = { model | clockSkewMillis = millis - Time.posixToMillis received, clockOffsetMinutes = offset, clockTimezone = zone, clockSynced = True }
                next = updateClock received synced
            in
            ( next, refreshDateSummaries model next )

        -- ----- auth -----
        LoginEmailChanged v ->
            ( { model | loginEmail = v, loginError = "" }, Cmd.none )

        LoginPasswordChanged v ->
            ( { model | loginPassword = v, loginError = "" }, Cmd.none )

        LoginClicked ->
            if model.loginBusy then (model,Cmd.none)
            else if String.trim model.loginEmail == "" || model.loginPassword == "" then
                ( { model | loginError = "Enter your email address and password." }, Cmd.none )
            else ( { model | loginBusy = True, loginError = "" }
            , Api.login ctx model.loginEmail model.loginPassword model.loginOtp LoginDone
            )

        LoginDone (Ok resp) ->
            let
                authenticated = restoreSessionDraft resp.user.id
                    { model | token = Just resp.token, user = Just resp.user, authChecked = True, loginBusy = False, loginPassword = "", loginOtp = "", loginError = "", loginNotice = "" }
            in
            ( authenticated
            , Cmd.batch
                [ saveToken (Just resp.token)
                , toast "success" ("Welcome back, " ++ resp.user.name)
                , if resp.user.mfaRequired then Nav.pushUrl model.key "/work/security" else routeCmd authenticated (State.routeOf model.url)
                ]
            )

        LoginDone (Err e) ->
            ( { model | loginError = if e == Http.BadStatus 401 then "Check your email, password, and authenticator or recovery code." else httpError e, loginBusy = False }, Cmd.none )

        MeDone (Ok u) ->
            ( { model | user = Just u, authChecked = True }
            , if u.mfaRequired then Nav.pushUrl model.key "/work/security" else routeCmd model (State.routeOf model.url)
            )

        MeDone (Err e) ->
            ( { model | user = Nothing, authChecked = True, token = Nothing }
            , saveToken Nothing
            )

        LogoutClicked ->
            ( model, Api.logout ctx LogoutDone )

        -- ----- data loads -----
        LeadsLoaded result ->
            ( { model | leads = Result.withDefault model.leads result, pageLoading = False, pageError = resultError result }, Cmd.none )

        UsersLoaded result ->
            ( { model | users = Result.withDefault model.users result, pageLoading = False, pageError = resultError result }, Cmd.none )

        TasksLoaded result ->
            ( { model | tasks = Result.withDefault model.tasks result, pageLoading = False, pageError = resultError result }, Cmd.none )

        ClientsLoaded result ->
            ( { model | clients = Result.withDefault model.clients result, pageLoading = False, pageError = resultError result }, Cmd.none )

        ClientDetailLoaded result ->
            ( { model | clientDetail = justOf result, pageLoading = False, pageError = resultError result }
            , case result of
                Ok detail ->
                    Cmd.batch
                        [ Api.clientProfile ctx detail.client.id (acceptLoad model ClientProfileLoaded)
                        , Api.featureGet ctx (inlinePath model detail.client.id) model.featureSearch model.pageOffset (acceptLoad model FeatureLoaded)
                        , if List.isEmpty model.users then Api.fetchUsers ctx (acceptLoad model UsersLoaded) else Cmd.none
                        ]
                Err _ ->
                    Cmd.none
            )

        DetailLoaded result ->
            ( { model | detail = justOf result, pageLoading = False, pageError = resultError result }, Cmd.none )

        ClockStatsLoaded requestedDate result ->
            case result of
                Ok stats ->
                    if requestedDate == model.today then ( { model | stats = Just stats }, Cmd.none ) else ( model, Cmd.none )
                Err _ -> ( model, Cmd.none )

        StatsLoaded result ->
            ( { model | stats = justOf result, pageLoading = False, pageError = resultError result }, Cmd.none )

        AuditLoaded result ->
            ( { model | audit = Result.withDefault model.audit result }
            , Cmd.none
            )

        -- ----- lead CRUD -----
        OpenAddLead ->
            ( { model | showAddLead = True, addLead = State.emptyAddLead }, Cmd.none )

        CloseAddLead ->
            ( { model | showAddLead = False, formDirty = False }, Cmd.none )

        UpdateAddLead f ->
            ( { model | addLead = f model.addLead, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        SubmitAddLead -> checkContactBeforeSave "add-lead" model

        AddLeadDone (Ok lead) ->
            ( { model | showAddLead = False, featureBusy = False, formDirty = False, leads = lead :: model.leads }
            , Cmd.batch
                [ toast "success" ("Lead " ++ lead.number ++ " created")
                , navigateTo model.key "/leads"
                ]
            )

        AddLeadDone (Err e) ->
            ( { model | featureBusy = False }, toast "error" (httpError e) )

        EditLeadToggled ->
            case model.detail of
                Nothing ->
                    ( model, Cmd.none )

                Just d ->
                    ( { model
                        | editLeadOpen = not model.editLeadOpen
                        , editLead = State.editLeadOf d.lead
                        , recordBaseline = Types.encodeUpdateLead (State.editLeadOf d.lead)
                      }
                    , Cmd.none
                    )

        UpdateEditLead f ->
            ( { model | editLead = f model.editLead, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        SubmitEditLead -> checkContactBeforeSave "edit-lead" model

        EditLeadDone (Ok lead) ->
            ( { model | editLeadOpen = False, featureBusy = False, formDirty = False }
            , Cmd.batch
                [ toast "success" "Lead updated"
                  -- refresh detail + list quietly
                , reloadCurrent model
                ]
            )

        EditLeadDone (Err (Http.BadStatus 409)) ->
            beginConflict { model | featureBusy = False } "leads" (Maybe.map (.lead >> .id) model.detail |> Maybe.withDefault 0) model.recordBaseline (Types.encodeUpdateLead model.editLead)

        EditLeadDone (Err e) ->
            ( { model | featureBusy = False }, toast "error" (httpError e) )

        DeleteLeadConfirmed id ->
            ( model, Api.deleteLead ctx id DeleteLeadDone )

        DeleteLeadDone (Ok _) ->
            ( model
            , Cmd.batch [ toast "success" "Lead deleted", navigateTo model.key "/leads" ]
            )

        DeleteLeadDone (Err e) ->
            ( model, toast "error" (httpError e) )

        AdvanceClicked id ->
            ( model, Api.advanceLead ctx id AdvanceDone )

        AdvanceDone (Ok lead) ->
            ( model
            , Cmd.batch
                [ toast "success" ("Advanced to " ++ lead.stage)
                , reloadCurrent model
                ]
            )

        AdvanceDone (Err e) ->
            ( model, toast "error" (httpError e) )

        ClientProfileLoaded result ->
            case result of
                Ok value -> ( { model | clientProfile = value, clientPhotoError = "" }, Cmd.none )
                Err err -> ( { model | clientPhotoError = httpError err }, Cmd.none )

        ClientPhotoPicked file ->
            if File.size file <= 0 || File.size file > 2 * 1024 * 1024 || not (List.member (File.mime file) ["image/jpeg","image/png"]) then
                ( { model | clientPhotoFile = Nothing, clientPhotoPreview = Nothing, clientPhotoError = "Choose a PNG or JPEG up to 2 MB and 4096 × 4096 pixels." }, Cmd.none )
            else
                ( { model | clientPhotoFile = Just file, clientPhotoPreview = Nothing, clientPhotoError = "", clientPhotoNotice = "" }, Task.perform (ClientPhotoPreviewReady file) (File.toUrl file) )

        ClientPhotoPreviewReady file url ->
            if model.clientPhotoFile == Just file then
                ( { model | clientPhotoPreview = Just url }, Cmd.none )
            else
                ( model, Cmd.none )

        ClientPhotoUpload cid ->
            case model.clientPhotoFile of
                Nothing -> ( { model | clientPhotoError = "Choose a student photo first." }, Cmd.none )
                Just file ->
                    let expected = D.decodeValue (D.at ["photo","documentId"] D.int) model.clientProfile |> Result.withDefault 0 in
                    ( { model | clientPhotoBusy = True, clientPhotoNotice = "", clientPhotoError = "" }, Api.uploadClientPhoto ctx cid expected file (acceptLoad model (ClientPhotoDone cid)) )

        ClientPhotoDone cid result ->
            case result of
                Ok value ->
                    ( { model | clientPhotoBusy = False, clientPhotoFile = Nothing, clientPhotoPreview = Nothing, clientPhotoNotice = Features.str "message" value }, Api.clientProfile ctx cid (acceptLoad model ClientProfileLoaded) )
                Err err ->
                    ( { model | clientPhotoBusy = False, clientPhotoError = if err == Http.BadStatus 400 then "Use a valid PNG or JPEG up to 2 MB and 4096 × 4096 pixels." else httpError err }, Cmd.none )

        ClientProfileRefresh cid ->
            ( model, Api.clientProfile ctx cid (acceptLoad model ClientProfileLoaded) )

        ClientPhotoRetry cid ->
            ( { model | clientPhotoBusy = True, clientPhotoError = "" }, Api.retryClientPhoto ctx cid (acceptLoad model (ClientPhotoDone cid)) )

        ConvertClicked id ->
            ( { model | featureBusy = True }, Api.convertLead ctx id (acceptLoad model ConvertDone) )

        ConvertDone (Ok client) ->
            ( { model | featureBusy = False, formDirty = False }
            , Cmd.batch
                [ toast "success" ("Converted to student " ++ client.number)
                , navigateTo model.key ("/clients/" ++ String.fromInt client.id)
                ]
            )

        ConvertDone (Err e) ->
            ( { model | featureBusy = False }, toast "error" (httpError e) )

        NoteDraftChanged v ->
            ( { model | noteDraft = v, formDirty = True }, Cmd.none )

        NoteSubmitted id ->
            if String.trim model.noteDraft == "" then
                ( model, Cmd.none )

            else
                ( model, Api.addNote ctx id model.noteDraft NoteDone )

        NoteDone (Ok _) ->
            ( { model | noteDraft = "", formDirty = False }
            , Cmd.batch [ toast "success" "Note added", reloadCurrent model ]
            )

        NoteDone (Err e) ->
            ( model, toast "error" (httpError e) )

        -- ----- filters / bulk -----
        SearchChanged v ->
            ( { model | q = v, pageOffset = 0 }, Task.perform (\_ -> SearchReady "leads" v) (Process.sleep 300) )

        StageFilterChanged v ->
            ( { model | fStage = v, pageOffset = 0, pageLoading = True }, Api.fetchFilteredLeads ctx { model | fStage = v } 0 (acceptLoad ({ model | fStage = v, pageOffset = 0 }) LeadsLoaded) )

        QualityFilterChanged v ->
            ( { model | fQuality = v, pageOffset = 0, pageLoading = True }, Api.fetchFilteredLeads ctx { model | fQuality = v } 0 (acceptLoad ({ model | fQuality = v, pageOffset = 0 }) LeadsLoaded) )

        SourceFilterChanged v ->
            ( { model | fSource = v, pageOffset = 0, pageLoading = True }, Api.fetchFilteredLeads ctx { model | fSource = v } 0 (acceptLoad ({ model | fSource = v, pageOffset = 0 }) LeadsLoaded) )

        SortByChanged v ->
            ( { model | sortBy = v, pageOffset = 0, pageLoading = True }, Api.fetchFilteredLeads ctx { model | sortBy = v } 0 (acceptLoad ({ model | sortBy = v, pageOffset = 0 }) LeadsLoaded) )

        SortDirToggled ->
            ( { model | sortAsc = not model.sortAsc, pageOffset = 0 }, Api.fetchFilteredLeads ctx { model | sortAsc = not model.sortAsc } 0 (acceptLoad ({ model | sortAsc = not model.sortAsc, pageOffset = 0 }) LeadsLoaded) )

        ClearFilters ->
            let next = { model | q = "", fStage = "All", fQuality = "All", fSource = "All", pageOffset = 0, pageLoading = True, selected = Set.empty } in
            ( next, Api.fetchFilteredLeads ctx next 0 (acceptLoad next LeadsLoaded) )

        RowChecked id flag ->
            ( { model
                | selected =
                    if flag then
                        Set.insert id model.selected

                    else
                        Set.remove id model.selected
              }
            , Cmd.none
            )

        SelectAllChecked flag ->
            ( { model
                | selected =
                    if flag then
                        Set.fromList (List.map .id (Views.filteredLeads model))

                    else
                        Set.empty
              }
            , Cmd.none
            )

        BulkAdvanceClicked ->
            ( model
            , Api.bulkLeads ctx
                { op = "advance", ids = Set.toList model.selected, consultantId = Nothing }
                BulkDone
            )

        BulkAssignPicked userIdStr ->
            case String.toInt userIdStr of
                Nothing ->
                    ( model, Cmd.none )

                Just uid ->
                    ( model
                    , Api.bulkLeads ctx
                        { op = "assign", ids = Set.toList model.selected, consultantId = Just uid }
                        BulkDone
                    )

        BulkDeleteClicked ->
            ( model
            , Api.bulkLeads ctx
                { op = "delete", ids = Set.toList model.selected, consultantId = Nothing }
                BulkDone
            )

        BulkDone (Ok _) ->
            ( { model | selected = Set.empty }
            , Cmd.batch [ toast "success" "Bulk action applied", reloadCurrent model ]
            )

        BulkDone (Err e) ->
            ( model, toast "error" (httpError e) )

        -- ----- tasks -----
        TaskStatusPicked id status ->
            let
                allTasks = model.tasks ++ (Maybe.map .tasks model.detail |> Maybe.withDefault []) ++ (Maybe.map .tasks model.clientDetail |> Maybe.withDefault [])
                revision = allTasks |> List.filter (\task -> task.id == id) |> List.head |> Maybe.map .revision |> Maybe.withDefault 0
            in
            ( model, Api.setTaskStatus ctx id revision status TaskStatusDone )

        TaskStatusDone (Ok _) ->
            ( model, Cmd.batch [ toast "success" "Task updated", reloadCurrent model ] )

        TaskStatusDone (Err e) ->
            ( model, toast "error" (httpError e) )

        TaskDeleted id ->
            ( model, Api.deleteTask ctx id TaskDeleteDone )

        TaskDeleteDone (Ok _) ->
            ( model, Cmd.batch [ toast "success" "Task deleted", reloadCurrent model ] )

        TaskDeleteDone (Err e) ->
            ( model, toast "error" (httpError e) )

        ToggleGlobalAddTask ->
            ( { model
                | showGlobalAddTask = not model.showGlobalAddTask
                , addTask = State.emptyAddTask
                , featureErrors = Dict.empty
                , featureError = ""
              }
            , Cmd.none
            )

        UpdateAddTask f ->
            ( { model | addTask = f model.addTask, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        SubmitAddTaskFor leadId ->
            submitTask model ctx leadId

        SubmitClientTask cid ->
            ( model, Api.createTask ctx (clientTaskBody model cid) AddTaskDone )

        SubmitAddTaskGlobal ->
            let
                form = model.addTask
                own = if String.startsWith "client:" form.leadId then form.leadId else if form.leadId == "" then "" else "lead:" ++ form.leadId
                values = Dict.fromList [("owner",own),("title",form.title),("description",form.description),("dueDate",form.dueDate),("priority",form.priority),("assignee",form.assignee),("recurrenceDays",form.recurrenceDays),("status","Pending")]
                errors = Validation.feature "tasks" "create" "" values
            in
            if model.featureBusy then ( model, Cmd.none )
            else if not (Dict.isEmpty errors) then
                ( { model | featureErrors = errors, featureError = "Check the highlighted task fields." }, Cmd.none )
            else
                ( { model | featureBusy = True, featureError = "", featureErrors = Dict.empty }, Api.featurePost ctx "tasks/create" (dictValue values) (acceptLoad model FeatureDone) )

        AddTaskDone (Ok task) ->
            ( { model | showGlobalAddTask = False, formDirty = False, addTask = State.emptyAddTask }
            , Cmd.batch
                [ toast "success" "Task created"
                , reloadCurrent model
                ]
            )

        AddTaskDone (Err e) ->
            ( model, toast "error" (httpError e) )

        -- ----- clients -----
        ClientSearchChanged v ->
            ( { model | clientQ = v, pageOffset = 0 }, Task.perform (\_ -> SearchReady "clients" v) (Process.sleep 300) )

        OpenAddClient ->
            ( { model
                | showClientForm = True
                , clientEditingId = Nothing
                , clientForm = State.emptyClientForm
              }
            , Cmd.none
            )

        OpenClientEditor _ ->
            case model.clientDetail of
                Nothing ->
                    ( model, Cmd.none )

                Just cd ->
                    ( { model
                        | showClientForm = True
                        , clientEditingId = Just cd.client.id
                        , clientForm = State.clientFormOf cd.client
                        , recordBaseline = clientPayload { model | clientForm = State.clientFormOf cd.client }
                      }
                    , Cmd.none
                    )

        CloseClientForm ->
            ( { model | showClientForm = False, featureBusy = False, formDirty = False }, Cmd.none )

        UpdateClientForm f ->
            ( { model | clientForm = f model.clientForm, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        SubmitClientForm -> checkContactBeforeSave "student" model

        ClientUpdated (Ok _) ->
            ( { model | showClientForm = False, featureBusy = False, formDirty = False }
            , Cmd.batch [ toast "success" "Student saved", reloadCurrent model ]
            )

        ClientUpdated (Err (Http.BadStatus 409)) ->
            beginConflict { model | featureBusy = False } "clients" (Maybe.withDefault 0 model.clientEditingId) model.recordBaseline (clientPayload model)

        ClientUpdated (Err e) ->
            ( { model | featureBusy = False }, toast "error" (httpError e) )

        ClientFormSaved (Ok _) ->
            ( { model | showClientForm = False, featureBusy = False, formDirty = False }
            , Cmd.batch [ toast "success" "Student saved", reloadCurrent model ]
            )

        ClientFormSaved (Err e) ->
            ( { model | featureBusy = False }, toast "error" (httpError e) )

        DeleteClientConfirmed id ->
            ( model
            , Api.deleteClient ctx id DeleteClientDone
            )

        DeleteClientDone (Ok _) ->
            ( model
            , Cmd.batch [ toast "success" "Student deleted", navigateTo model.key "/clients" ]
            )

        DeleteClientDone (Err e) ->
            ( model, toast "error" (httpError e) )

        OpenCaseForm mcase ->
            ( { model
                | recordBaseline = Maybe.map (State.caseFormOf >> casePayload) mcase |> Maybe.withDefault E.null
                , caseForm =
                    Just
                        (Maybe.map State.caseFormOf mcase
                            |> Maybe.withDefault State.emptyCaseForm
                        )
              }
            , Cmd.none
            )

        CloseCaseForm ->
            ( { model | caseForm = Nothing, formDirty = False }, Cmd.none )

        UpdateCaseForm f ->
            ( { model | caseForm = Maybe.map f model.caseForm, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        SubmitCaseForm clientId ->
            case model.caseForm of
                Nothing ->
                    ( model, Cmd.none )

                Just cf ->
                    ( model
                    , Api.upsertCase ctx clientId
                        (withRevision cf.revision (Types.encodeCaseInput
                            { category = cf.category
                            , destination = cf.destination
                            , stage = cf.stage
                            , deadline = cf.deadline
                            , caseId = cf.caseId
                            }
                        ))
                        CaseFormSaved
                    )

        CaseFormSaved (Ok cd) ->
            ( { model | caseForm = Nothing, formDirty = False, clientDetail = Just cd }
            , Cmd.batch [toast "success" "Application saved",reloadCurrent model]
            )

        CaseFormSaved (Err (Http.BadStatus 409)) ->
            beginConflict model "cases" (Maybe.andThen .caseId model.caseForm |> Maybe.withDefault 0) model.recordBaseline (Maybe.map casePayload model.caseForm |> Maybe.withDefault E.null)

        CaseFormSaved (Err e) ->
            ( model, toast "error" (httpError e) )

        DeleteCaseClicked caseId ->
            ( model, Api.deleteCase ctx caseId DeleteCaseDone )

        DeleteCaseDone (Ok _) ->
            let next = { model | inlineOwner = ownerForUrl model.url, featureForm = Dict.insert "owner" (ownerForUrl model.url) (Features.defaults model.inlineSection) } in
            ( next, Cmd.batch [ toast "success" "Application deleted", reloadCurrent next ] )

        DeleteCaseDone (Err e) ->
            ( model, toast "error" (httpError e) )

        -- ----- documents -----
        DocPicked file ->
            ( { model | pendingDoc = Just file }, Cmd.none )

        DocCleared ->
            ( { model | pendingDoc = Nothing }, Cmd.none )

        UploadLeadDocClicked id ->
            case model.pendingDoc of
                Nothing ->
                    ( model, toast "error" "Choose a file first" )

                Just file ->
                    if model.documentBusy then ( model, Cmd.none ) else if File.size file <= 0 || File.size file > 25 * 1024 * 1024 then ( model, toast "error" "Choose a nonempty file up to 25 MB" ) else ( { model | documentBusy = True }, Api.uploadLeadDocument ctx id file LeadDocUploaded )

        UploadClientDocClicked id ->
            case model.pendingDoc of
                Nothing ->
                    ( model, toast "error" "Choose a file first" )

                Just file ->
                    if model.documentBusy then ( model, Cmd.none ) else if File.size file <= 0 || File.size file > 25 * 1024 * 1024 then ( model, toast "error" "Choose a nonempty file up to 25 MB" ) else ( { model | documentBusy = True }, Api.uploadClientDocument ctx id file ClientDocUploaded )

        LeadDocUploaded (Ok detail) ->
            ( { model | detail = Just detail, pendingDoc = Nothing, documentBusy = False }
            , toast "success" "File uploaded. Scanning must finish before download or approval."
            )

        LeadDocUploaded (Err e) ->
            ( { model | documentBusy = False }, toast "error" (httpError e) )

        ClientDocUploaded (Ok cd) ->
            ( { model | clientDetail = Just cd, pendingDoc = Nothing, documentBusy = False }
            , Cmd.batch [toast "success" "Document uploaded for malware scanning",reloadCurrent model]
            )

        ClientDocUploaded (Err e) ->
            ( { model | documentBusy = False }, toast "error" (httpError e) )

        DocDeleted id ->
            ( model, Api.deleteDocument ctx id DocDeleteDone )

        DocDeleteDone (Ok _) ->
            ( model
            , Cmd.batch [ toast "success" "Document deleted", reloadCurrent model ]
            )

        DocDeleteDone (Err e) ->
            ( model, toast "error" (httpError e) )

        -- ----- csv import -----
        ImportFilePicked file ->
            ( { model | pendingImport = Just file }, Cmd.none )

        LeadImportClicked ->
            case model.pendingImport of
                Nothing ->
                    ( model, toast "error" "Choose a CSV file first" )

                Just file ->
                    ( model, Api.importLeads ctx file LeadsImported )

        LeadsImported (Ok res) ->
            let
                failCount =
                    List.length res.failed

                failureSummary =
                    res.failed
                        |> List.take 3
                        |> List.map (\fe -> "row " ++ String.fromInt fe.row ++ ": " ++ fe.error)
                        |> String.join "; "

                failToast =
                    if failCount == 0 then
                        Cmd.none

                    else
                        toast "error"
                            ("Rows failed ("
                                ++ String.fromInt failCount
                                ++ ") — "
                                ++ failureSummary
                            )
            in
            ( { model | pendingImport = Nothing }
            , Cmd.batch
                [ toast "success"
                    ("Imported "
                        ++ String.fromInt res.imported
                        ++ " lead"
                        ++ (if res.imported == 1 then
                                ""

                            else
                                "s"
                           )
                    )
                , failToast
                , reloadCurrent model
                ]
            )

        LeadsImported (Err e) ->
            ( model, toast "error" (httpError e) )

        -- ----- users / account -----
        OpenAddUser ->
            ( { model | showUserForm = True, userForm = State.emptyUserForm }
            , Cmd.none
            )

        OpenUserEditor uid ->
            case List.filter (\u -> u.id == uid) model.users of
                (u :: _) ->
                    ( { model | showUserForm = True, userForm = State.userFormOf u, recordBaseline = E.object [("uuName",E.string u.name),("uuRole",E.string u.role)] }
                    , Cmd.none
                    )

                [] ->
                    ( model, Cmd.none )

        CloseUserForm ->
            ( { model | showUserForm = False, formDirty = False }, Cmd.none )

        UpdateUserForm f ->
            ( { model | userForm = f model.userForm, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        SubmitUserForm ->
            let
                f =
                    model.userForm
            in
            case f.userId of
                Nothing ->
                    if f.password /= "" && String.length f.password < 12 then
                        ( model, toast "error" "Password must be at least 12 characters" )

                    else
                        ( model
                        , Api.createUser ctx
                            (Types.encodeNewUser
                                { name = f.name
                                , email = f.email
                                , role = f.role
                                , password = f.password
                                }
                            )
                            UserSaved
                        )

                Just uid ->
                    ( model
                    , Api.updateUser ctx uid
                        (withRevision f.revision (Types.encodeUpdateUser
                            { name = f.name
                            , role = f.role
                            , password = f.password
                            }
                        ))
                        UserUpdatedDone
                    )

        UserSaved (Ok u) ->
            ( { model | showUserForm = False, formDirty = False, users = model.users ++ [ u ] }
            , Cmd.batch [ toast "success" ("User " ++ u.name ++ " created"), reloadCurrent model ]
            )

        UserSaved (Err e) ->
            ( model, toast "error" (httpError e) )

        UserUpdatedDone (Ok _) ->
            ( { model | showUserForm = False, formDirty = False }
            , Cmd.batch [ toast "success" "User saved", reloadCurrent model ]
            )

        UserUpdatedDone (Err (Http.BadStatus 409)) ->
            beginConflict model "users" (Maybe.withDefault 0 model.userForm.userId) model.recordBaseline (E.object [("uuName",E.string model.userForm.name),("uuRole",E.string model.userForm.role)])

        UserUpdatedDone (Err e) ->
            ( model, toast "error" (httpError e) )

        DeleteUserConfirmed uid ->
            ( model, Api.deleteUser ctx uid DeleteUserDone )

        DeleteUserDone (Ok _) ->
            ( model
            , Cmd.batch [ toast "success" "User deleted", reloadCurrent model ]
            )

        DeleteUserDone (Err e) ->
            ( model, toast "error" (httpError e) )

        TogglePwModal ->
            ( { model
                | showPwModal = not model.showPwModal
                , pwCurrent = ""
                , pwNew = ""
                , pwConfirm = ""
              }
            , Cmd.none
            )

        PwCurrentChanged v ->
            ( { model | pwCurrent = v, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        PwNewChanged v ->
            ( { model | pwNew = v, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        PwConfirmChanged v ->
            ( { model | pwConfirm = v, formDirty = True, featureErrors = Dict.empty }, Cmd.none )

        SubmitPasswordChange ->
            let
                validationError =
                    if String.length model.pwNew < 12 then
                        "New password must be at least 12 characters"

                    else if model.pwNew /= model.pwConfirm then
                        "Passwords do not match"

                    else
                        ""
            in
            if validationError /= "" then
                ( model, toast "error" validationError )

            else
                ( model
                , Api.changePassword ctx model.pwCurrent model.pwNew PasswordChangeDone
                )

        PasswordChangeDone (Ok _) ->
            ( { model
                | showPwModal = False
                , loginNotice = "Your password was updated. Sign in with your new password."
                , pwCurrent = ""
                , pwNew = ""
                , pwConfirm = ""
              }
            , Cmd.batch [ Api.logout ctx LogoutDone, toast "success" "Password updated. Sign in again." ]
            )

        PasswordChangeDone (Err e) ->
            ( model, toast "error" (httpError e) )

        -- ----- chrome -----
        NotifBellClicked ->
            ( { model | notifOpen = not model.notifOpen }, Cmd.none )

        NavigateTo path ->
            ( { model | notifOpen = False, pageError = "" }, if path == model.url.path then reloadCurrent model else Nav.pushUrl model.key path )

        ToastPushed kind textMsg ->
            pushToast model kind textMsg

        ToastExpired id ->
            ( { model | toasts = List.filter (\t -> t.id /= id) model.toasts }
            , Cmd.none
            )



-- ---------- small helpers used by update ----------


submitTask : Model -> Api.Ctx -> Int -> ( Model, Cmd Msg )
submitTask model ctx leadId =
    if String.trim model.addTask.title == "" then
        ( model, Cmd.none )

    else
        ( model
        , Api.createTask ctx
            (Types.encodeNewTask
                { leadId = leadId
                , title = model.addTask.title
                , description = model.addTask.description
                , dueDate = model.addTask.dueDate
                , priority = model.addTask.priority
                }
            )
            AddTaskDone
        )


reloadCurrent : Model -> Cmd Msg
reloadCurrent model =
    routeCmd model (State.routeOf model.url)


reloadPanel : Model -> Cmd Msg
reloadPanel model =
    case ( State.routeOf model.url, model.clientDetail ) of
        ( ClientDetailPage cid, Just detail ) ->
            if detail.client.id == cid then
                Api.featureGet { base = model.apiBase, token = model.token } (inlinePath model cid) model.featureSearch model.pageOffset (acceptLoad model FeatureLoaded)
            else
                reloadCurrent model

        _ ->
            reloadCurrent model


clientPayload : Model -> E.Value
clientPayload model =
    withRevision model.clientForm.revision (Types.encodeNewClient
        { fullName = model.clientForm.fullName
        , email = model.clientForm.email
        , phone = model.clientForm.phone
        , officerId = String.toInt model.clientForm.officerId
        })


justOf : Result Http.Error a -> Maybe a
justOf r =
    Result.toMaybe r


toast : String -> String -> Cmd Msg
toast kind textMsg =
    Task.perform identity
        (Task.succeed (ToastPushed kind textMsg))


pushToast : Model -> String -> String -> ( Model, Cmd Msg )
pushToast model kind textMsg =
    let
        id =
            model.toastSeq + 1

        newToast =
            { id = id, kind = kind, msg = textMsg }
    in
    ( { model | toasts = model.toasts ++ [ newToast ], toastSeq = id }
    , Task.perform (always (ToastExpired id))
        (Process.sleep 3500)
    )


httpError : Http.Error -> String
httpError err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timed out"

        Http.NetworkError ->
            "Network error — is the API running?"

        Http.BadStatus code ->
            case code of
                401 ->
                    "Not authenticated — sign in again"

                403 ->
                    "You don't have permission for that"

                404 ->
                    "Not found"

                409 ->
                    "This record or action changed. Refresh and review the latest version before trying again."

                428 ->
                    "Reload this record to obtain its current version before saving."

                429 ->
                    "Too many requests. Wait before trying again."

                423 ->
                    "File quarantined: wait for a successful malware scan. Infected files and failed scans stay blocked."

                _ ->
                    "Server error (" ++ String.fromInt code ++ ")"

        Http.BadBody body ->
            String.left 240 body



-- ---------- view ----------


view : Model -> Browser.Document Msg
view model =
    { title = "ECC · Student workspace"
    , body =
        [ div
            [ class
                (if model.darkMode then
                    "app-root dark-mode"

                 else
                    "app-root"
                )
            ]
            (bodyView model)
        ]
    }


bodyView : Model -> List (Html Msg)
bodyView model =
    if model.url.path == "/portal" then
        [ Features.portalView model ]

    else if not model.authChecked then
        [ div [ class "login-wrap" ] [ Ui.spinner ] ]

    else if model.user == Nothing then
        [ Views.loginView model ]

    else
        List.concat
            [ [ a [ href "#main-content", class "skip-link" ] [ text "Skip to content" ], headerBar model
              , mobileNavigation model
              , Html.main_ [ class "container", Html.Attributes.id "main-content", Html.Attributes.tabindex -1 ] [ workspaceHeading model, if model.pageError /= "" then div [ class "error-banner", Html.Attributes.attribute "role" "alert" ] [ text model.pageError, button [ class "btn btn-sm btn-outline", onClick (NavigateTo model.url.path) ] [ text "Try again" ] ] else span [] [], if model.pageError /= "" && (model.detail == Nothing && model.clientDetail == Nothing) then span [] [] else pageView model, pagination model ]
              ]
            , if model.showUserForm then
                [ Views.userFormModal model ]

              else
                []
            , if model.showPwModal then
                [ Views.passwordModal model ]

              else
                []
            , [ Features.conflictView model ]
            , case model.confirmation of
                Just (message,action) -> [ contactConfirmation message action ]
                Nothing -> []
            , [ Views.toastStack model ]
            ]


pageView : Model -> Html Msg
pageView model =
    case State.routeOf model.url of
        Dashboard ->
            Views.dashboardView model

        LeadsPage ->
            Views.leadsView model

        LeadDetailPage id ->
            Views.leadDetailView model id

        ClientsPage ->
            Views.clientsView model

        ClientDetailPage id ->
            Views.clientDetailView model id

        TasksPage ->
            Views.tasksView model

        UsersPage ->
            Views.usersView model

        AuditPage ->
            Views.auditView model

        WorkspacePage kind ->
            div []
                [ workspaceSections model kind
                , if String.startsWith "work/" kind then Features.view model (String.dropLeft 5 kind) else Operations.view model kind
                ]


workspaceSections : Model -> String -> Html Msg
workspaceSections model kind =
    let
        isAdmin = Maybe.map .role model.user == Just "admin"
        (title, sections) =
            if List.member kind ["work/notifications", "reminders", "work/reminders"] then
                ("Inbox", [("work/notifications", "/work/notifications", "Updates"), ("reminders", "/reminders", "Upcoming work"), ("work/reminders", "/work/reminders", "Email rules")])
            else if isAdmin && List.member kind ["work/reports", "work/ageing", "work/bottlenecks", "work/trends"] then
                ("Reports", [("work/reports", "/work/reports", "Overview"), ("work/ageing", "/work/ageing", "Application ageing"), ("work/bottlenecks", "/work/bottlenecks", "Bottlenecks"), ("work/trends", "/work/trends", "Historical workload")])
            else if List.member kind ["work/invoices","work/sales","work/payments","work/payment-history","work/balances","work/expenses","work/cashflow","work/finance-audit","work/periods","work/reconciliation"] then
                ("Sales and accounting", (if isAdmin then [("work/invoices","/work/invoices","Invoices")] else []) ++ [("work/sales","/work/sales","Sales report"),("work/payments","/work/payments","Payments"),("work/balances","/work/balances","Balances"),("work/expenses","/work/expenses","Expenses"),("work/cashflow","/work/cashflow","Receipts and expenses"),("work/finance-audit","/work/finance-audit","Financial audit"),("work/periods","/work/periods","Period closing"),("work/reconciliation","/work/reconciliation","Bank reconciliation")])
            else if isAdmin && List.member kind ["duplicates", "work/merge"] then
                ("Review duplicates", [("duplicates", "/duplicates", "1. Find matches"), ("work/merge", "/work/merge", "2. Compare and merge")])
            else if isAdmin && List.member kind ["metrics", "work/mail"] then
                ("Administration", [("metrics", "/system", "System overview"), ("work/mail", "/work/mail", "Email delivery")])
            else
                ("", [])
    in
    if List.isEmpty sections then
        text ""
    else
        div [ class "workspace-sections" ]
            [ Html.h2 [] [ text title ]
            , Html.nav [ class "form-actions", Html.Attributes.attribute "aria-label" (title ++ " sections") ]
                (List.map (\(section, path, label) ->
                    a [ href path, class (if section == kind then "btn btn-primary" else "btn btn-outline"), Html.Attributes.attribute "aria-current" (if section == kind then "page" else "false") ] [ text label ]
                ) sections)
            ]


headerBar : Model -> Html Msg
headerBar model =
    let
        current =
            model.url.path

        navBtn path label =
            a
                [ class
                    (if isActive path then
                        "nav-link active"

                     else
                        "nav-link"
                    )
                , href path
                , Html.Attributes.attribute "aria-current" (if isActive path then "page" else "false")
                ]
                [ Ui.icon (if path == "/" then "dashboard" else if path == "/users" then "clients" else String.dropLeft 1 path), span [] [ text label ] ]

        isActive path =
            if path == "/" then
                current == "/"

            else if path == "/work/notifications" then
                List.member current ["/work/notifications", "/reminders", "/work/reminders"]

            else if path == "/work/payments" then
                List.member current ["/work/payments","/work/payment-history","/work/balances","/work/expenses","/work/cashflow","/work/finance-audit","/work/periods","/work/reconciliation"]
            else if path == "/work/reports" then
                List.member current ["/work/reports", "/work/ageing", "/work/bottlenecks", "/work/trends"]

            else if path == "/duplicates" then
                List.member current ["/duplicates", "/work/merge"]

            else if path == "/system" then
                List.member current ["/system", "/work/mail"]

            else
                String.startsWith path current

        user_ =
            Maybe.withDefault { id = 0, email = "", name = "", role = "", revision = 1, mfaRequired = False } model.user
    in
    div [ class "header" ]
        [ div [ class "logo" ]
            [ Html.img [ class "brand-emblem", Html.Attributes.src "/brand/ecc-emblem.png", Html.Attributes.alt "ECC Consulting Global", Html.Attributes.width 40, Html.Attributes.height 40 ] []
            , strong [] [ text "ECC CRM" ]
            ]
        , Html.nav [ class "nav", Html.Attributes.attribute "aria-label" "Main navigation" ]
            [ div [ class "nav-group" ]
                [ div [ class "nav-section-label" ] [ text "WORKSPACE" ]
                , navBtn "/" "Today"
                , navBtn "/leads" "Leads"
                , navBtn "/clients" "Students"
                , navBtn "/applications" "Applications"
                , navBtn "/tasks" "Tasks"
                ]
            , div [ class "nav-group" ]
                [ div [ class "nav-section-label" ] [ text "FOLLOW UP" ]
                , navBtn "/work/notifications" "Inbox"
                , navBtn "/work/documents" "Documents"
                ]
            , Html.details
                ([ class "workspace-tools" ]
                    ++ (if List.member current ["/work/sales","/work/payments","/work/payment-history","/work/balances","/work/expenses","/work/cashflow","/work/finance-audit","/work/periods","/work/reconciliation","/work/tasks","/work/appointments","/work/milestones","/work/contact-log","/work/invoices","/work/search","/work/communication","/work/portal","/work/security","/work/history"] then [ Html.Attributes.attribute "open" "" ] else [])
                )
                [ Html.summary [] [ text "More tools" ]
                , div [ class "tools-menu" ]
                    [ navBtn "/work/tasks" "Task assignments"
                    , navBtn "/work/history" "Notes and history"
                    , navBtn "/work/communication" "Send email"
                    , navBtn "/work/portal" "Student portal"
                    , navBtn "/work/appointments" "Appointments"
                    , navBtn "/work/milestones" "Milestones"
                    , navBtn "/work/contact-log" "Calls and messages"
                    , if user_.role == "admin" then navBtn "/work/invoices" "Invoices" else text ""
                    , navBtn "/work/sales" "Sales report"
                    , navBtn "/work/payments" "Payments and accounting"
                    , navBtn "/work/search" "Advanced search"
                    , navBtn "/work/security" "Account security"
                    ]
                ]
            , if user_.role == "admin" then
                div [ class "nav-group" ]
                    [ div [ class "nav-section-label" ] [ text "ADMINISTRATION" ]
                    , navBtn "/work/reports" "Reports"
                    , navBtn "/duplicates" "Review duplicates"
                    , navBtn "/users" "Team"
                    , navBtn "/work/checklist-templates" "Checklist templates"
                    , navBtn "/archive" "Archive"
                    , navBtn "/system" "System and delivery"
                    , navBtn "/audit" "Audit"
                    ]
              else
                text ""
            ]
        , div [ class "header-right" ]
            [ a [ href "/guide.html", Html.Attributes.target "_blank", Html.Attributes.rel "noopener", class "btn btn-sm btn-outline" ] [ text "Help" ]
            , button
                [ class "btn btn-sm btn-outline"
                , onClick TogglePwModal
                ]
                [ text "Password" ]
            , button [ class "btn btn-sm btn-outline dark-toggle", onClick ToggleDarkMode ]
                [ text
                    (if model.darkMode then
                        "☀ Light"

                     else
                        "☾ Dark"
                    )
                ]
            , span [ class "user-chip" ]
                [ Ui.avatar user_.name
                , span [ class "user-identity" ] [ strong [] [ text user_.name ], span [ class "user-role" ] [ text user_.role ] ]
                ]
            , button [ class "btn-logout", onClick LogoutClicked ] [ text "Log out" ]
            ]
        ]


mobileNavigation : Model -> Html Msg
mobileNavigation model =
    let
        current = model.url.path
        primary path label iconName =
            let
                active = if path == "/" then current == "/" else String.startsWith path current
            in
            a
                [ href path
                , class ("mobile-tab" ++ (if active then " is-active" else ""))
                , Html.Attributes.attribute "aria-current" (if active then "page" else "false")
                ]
                [ Ui.icon iconName, span [] [ text label ] ]

        tool path label =
            a [ href path, class "mobile-menu-link", Html.Attributes.attribute "aria-current" (if current == path then "page" else "false") ] [ text label, span [ Html.Attributes.attribute "aria-hidden" "true" ] [ text "›" ] ]

        user = Maybe.withDefault { id = 0, email = "", name = "", role = "", revision = 1, mfaRequired = False } model.user
    in
    Html.nav [ class "mobile-bottom-nav", Html.Attributes.attribute "aria-label" "Mobile navigation" ]
        [ primary "/" "Today" "dashboard"
        , primary "/leads" "Leads" "leads"
        , primary "/clients" "Students" "clients"
        , primary "/tasks" "Tasks" "tasks"
        , Html.details [ class ("mobile-more" ++ (if current /= "/" && not (List.any (\path -> String.startsWith path current) ["/leads", "/clients", "/tasks"]) then " is-active" else "")) ]
            [ Html.summary [ class "mobile-tab" ] [ Ui.icon "more", span [] [ text "More" ] ]
            , div [ class "mobile-menu-panel", Html.Attributes.attribute "aria-label" "More workspace tools" ]
                [ div [ class "mobile-menu-heading" ] [ strong [] [ text "Your workspace" ], span [] [ text user.name ] ]
                , div [ class "mobile-menu-grid" ]
                    [ tool "/applications" "Applications"
                    , tool "/work/notifications" "Inbox"
                    , tool "/work/documents" "Documents"
                    , tool "/work/appointments" "Appointments"
                    , tool "/work/milestones" "Milestones"
                    , tool "/work/contact-log" "Calls and messages"
                    , tool "/work/tasks" "Task assignments"
                    , tool "/work/history" "Notes and history"
                    , tool "/work/sales" "Sales report"
                    , tool "/work/payments" "Payments and accounting"
                    , tool "/work/payment-history" "Payment history"
                    , tool "/work/expenses" "Expenses"
                    , tool "/work/periods" "Period closing"
                    , tool "/work/reconciliation" "Bank reconciliation"
                    , tool "/work/search" "Advanced search"
                    , tool "/work/communication" "Send email"
                    , tool "/work/portal" "Student portal"
                    , tool "/work/security" "Account security"
                    ]
                , if user.role == "admin" then
                    div [ class "mobile-menu-grid mobile-menu-admin" ]
                        [ tool "/work/reports" "Reports"
                        , tool "/users" "Team"
                        , tool "/duplicates" "Review duplicates"
                        , tool "/work/checklist-templates" "Checklist templates"
                        , tool "/work/invoices" "Invoices"
                        , tool "/archive" "Archive"
                        , tool "/system" "System and delivery"
                        , tool "/audit" "Audit"
                        ]
                  else text ""
                , div [ class "mobile-menu-account" ]
                    [ a [ href "/guide.html", Html.Attributes.target "_blank", Html.Attributes.rel "noopener", class "btn btn-outline" ] [ text "Help and guides" ]
                    , button [ class "btn btn-outline", onClick TogglePwModal ] [ text "Password" ]
                    , button [ class "btn btn-outline", onClick ToggleDarkMode ] [ text (if model.darkMode then "Light mode" else "Dark mode") ]
                    , button [ class "btn btn-outline", onClick LogoutClicked ] [ text "Log out" ]
                    ]
                ]
            ]
        ]



-- ---------- subscriptions ----------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 ClockTick
        , Time.every 60000 ClockRefresh
        , Browser.Events.onVisibilityChange (always ClockRefreshRequested)
        , loadToken GotStoredToken
        , loadReportLayout ReportSettingsLoaded
        , loadDark GotStoredDark
        ]



-- ports for JS glue (defined above; repeated imports kept minimal)


navigateTo : Nav.Key -> String -> Cmd msg
navigateTo key path =
    Nav.pushUrl key path


resultError : Result Http.Error a -> String
resultError result = case result of
    Ok _ -> ""
    Err err -> httpError err

workspaceHeading : Model -> Html Msg
workspaceHeading model =
    let
        (title,subtitle) = case State.routeOf model.url of
            Dashboard -> ("A clear view of what’s next.","Your relationships, progress and priorities in one place.")
            LeadsPage -> ("Build your next relationship.","Keep every conversation moving toward a clear next step.")
            ClientsPage -> ("Great service starts here.","A shared view of every student and their journey.")
            TasksPage -> ("Make room for your best work.","Turn follow-ups and deadlines into progress.")
            UsersPage -> ("Your people, connected.","Invite colleagues and manage their access.")
            WorkspacePage "work/applications" -> ("Every application, moving forward.","Manage each student’s applications, stages and next deadlines.")
            ClientDetailPage _ -> ("Student record","Applications, documents, tasks and history in one place.")
            AuditPage -> ("Every change, accounted for.","A complete history of activity across your workspace.")
            _ -> ("Your student workspace.","Keep the details close and the next step clear.")
        section = case State.routeOf model.url of
            Dashboard -> "Overview"
            LeadsPage -> "Leads"
            LeadDetailPage _ -> "Lead details"
            ClientsPage -> "Students"
            ClientDetailPage _ -> "Student details"
            TasksPage -> "Tasks"
            UsersPage -> "Team"
            AuditPage -> "Audit history"
            WorkspacePage kind ->
                if String.startsWith "work/" kind then
                    Tuple.first (Features.heading (String.dropLeft 5 kind))
                else if kind == "metrics" then
                    "System"
                else
                    String.toUpper (String.left 1 kind) ++ String.dropLeft 1 kind
    in
    div [ class "workspace-heading" ]
        [ div [ class "workspace-topline" ]
            [ div [ class "breadcrumb" ] [ a [ href "/" ] [ text "Workspace" ], span [] [ text "/" ], span [] [ text section ] ]
            , clockView model
            ]
        , div [ class "heading-content" ]
            [ div [] [ div [ class "eyebrow" ] [ text "ECC / STUDENT OPERATIONS" ], Html.h1 [] [ text title ], Html.p [] [ text subtitle ] ]
            , if State.routeOf model.url == Dashboard then a [ class "btn btn-primary heading-action", href "/leads" ] [ text "View pipeline", Ui.icon "arrow" ] else span [] []
            ]
        ]

clockView : Model -> Html Msg
clockView model =
    let
        pad number = String.padLeft 2 '0' (String.fromInt number)
        minutes = abs model.clockOffsetMinutes
        offset = (if model.clockOffsetMinutes < 0 then "-" else "+") ++ pad (minutes // 60) ++ ":" ++ pad (modBy 60 minutes)
        ready = model.today /= "" && model.currentTime /= ""
        timestamp = if ready then [ Html.Attributes.attribute "datetime" (model.today ++ "T" ++ model.currentTime ++ offset) ] else []
    in
    div [ class ("workspace-date workspace-clock" ++ (if model.clockSynced then " is-synced" else " is-unsynced")), Html.Attributes.attribute "aria-label" "Current business date and time", Html.Attributes.attribute "aria-live" "off" ]
        [ span [ class "clock-label" ] [ Ui.icon "clock", text "Current date and time" ]
        , Html.time (class "clock-reading" :: timestamp)
            [ strong [ class "clock-date" ] [ text (if ready then model.today else "Loading date…") ]
            , strong [ class "clock-time" ] [ text (if ready then model.currentTime else "--:--:--") ]
            ]
        , span [ class "clock-zone" ] [ text (model.clockTimezone ++ " · UTC" ++ offset) ]
        , span [ class "clock-status" ] [ text (if model.clockSynced then "Synced with server" else "Time not synchronized") ]
        ]


pagination : Model -> Html Msg
pagination model =
    let
        size = case State.routeOf model.url of
            LeadsPage -> Just (List.length model.leads)
            ClientsPage -> Just (List.length model.clients)
            TasksPage -> Just (List.length model.tasks)
            WorkspacePage kind -> if List.member kind ["metrics","work/reports","work/ageing","work/bottlenecks","work/trends","work/sales","work/cashflow"] then Nothing else Just (List.length model.opsData)
            _ -> Nothing
    in
    case size of
        Nothing -> span [] []
        Just count -> div [ class "pagination", Html.Attributes.attribute "aria-label" "Pagination" ]
            [ span [] [ text (if model.pageLoading then "Loading records…" else String.fromInt count ++ " records · page " ++ String.fromInt (1 + model.pageOffset // 50)) ]
            , button [ class "btn btn-outline", Html.Attributes.disabled (model.pageOffset == 0 || model.pageLoading), onClick (PageStep -1) ] [ text "← Previous" ]
            , button [ class "btn btn-outline", Html.Attributes.disabled (count < 50 || model.pageLoading), onClick (PageStep 1) ] [ text "Next →" ]
            ]


expiredSession : Msg -> Bool
expiredSession msg =
    case msg of
        ContactCheckDone _ _ _ (Err (Http.BadStatus 401)) -> True
        ConflictLoaded (Err (Http.BadStatus 401)) -> True
        FeatureDone (Err (Http.BadStatus 401)) -> True
        FeatureLoaded (Err (Http.BadStatus 401)) -> True
        TaskEditorLoaded (Err (Http.BadStatus 401)) -> True
        ClientProfileLoaded (Err (Http.BadStatus 401)) -> True
        ClientPhotoDone _ (Err (Http.BadStatus 401)) -> True
        LeadsLoaded (Err (Http.BadStatus 401)) -> True
        ClientsLoaded (Err (Http.BadStatus 401)) -> True
        TasksLoaded (Err (Http.BadStatus 401)) -> True
        DetailLoaded (Err (Http.BadStatus 401)) -> True
        ClientDetailLoaded (Err (Http.BadStatus 401)) -> True
        ClockStatsLoaded _ (Err (Http.BadStatus 401)) -> True
        StatsLoaded (Err (Http.BadStatus 401)) -> True
        OperationsLoaded (Err (Http.BadStatus 401)) -> True
        OperationDone (Err (Http.BadStatus 401)) -> True
        UsersLoaded (Err (Http.BadStatus 401)) -> True
        AuditLoaded (Err (Http.BadStatus 401)) -> True
        _ -> False


clientTaskBody : Model -> Int -> E.Value
clientTaskBody model cid =
    let form = model.addTask in
    E.object
        [ ("ntLeadId", E.null)
        , ("ntClientId", E.int cid)
        , ("ntTitle", E.string form.title)
        , ("ntDescription", E.string form.description)
        , ("ntDueDate", if form.dueDate == "" then E.null else E.string form.dueDate)
        , ("ntPriority", E.string form.priority)
        ]

featureDefaultsForUrl : Url -> Dict.Dict String String
featureDefaultsForUrl url =
    let defaults = Features.defaults (sectionForUrl url) in
    if sectionForUrl url == "applications" then
        Dict.get "client" (State.applicationFiltersForUrl url) |> Maybe.map (\cid -> Dict.insert "owner" ("client:" ++ cid) defaults) |> Maybe.withDefault defaults
    else defaults


sectionForUrl : Url -> String
sectionForUrl url =
    case State.routeOf url of
        WorkspacePage kind -> if String.startsWith "work/" kind then String.dropLeft 5 kind else kind
        _ -> "tasks"


featureSection : Model -> String
featureSection model =
    case State.routeOf model.url of
        ClientDetailPage _ -> model.inlineSection
        TasksPage -> "tasks"
        WorkspacePage kind -> if String.startsWith "work/" kind then String.dropLeft 5 kind else kind
        _ -> ""

withRevision : Int -> E.Value -> E.Value
withRevision revision value =
    E.object (("expectedRevision",E.int revision) :: (D.decodeValue (D.keyValuePairs D.value) value |> Result.withDefault []))

ownerForUrl : Url -> String
ownerForUrl url = case State.routeOf url of
    ClientDetailPage cid -> "client:" ++ String.fromInt cid
    _ -> ""

inlinePath : Model -> Int -> String
inlinePath model cid =
    "record/" ++ (if String.startsWith "case:" model.inlineOwner then String.replace ":" "/" model.inlineOwner else "client/" ++ String.fromInt cid) ++ "/" ++ model.inlineSection

dictValue : Dict.Dict String String -> E.Value
dictValue fields = E.object (Dict.toList fields |> List.map (Tuple.mapSecond E.string))

beginConflict : Model -> String -> Int -> E.Value -> E.Value -> (Model,Cmd Msg)
beginConflict model kind rid original edited =
    ( { model | conflictKind = kind, conflictCurrent = E.null, conflictOriginal = Features.formValues original, conflictEdits = Features.formValues edited }
    , Api.featureGet {base=model.apiBase,token=model.token} ("conflict/" ++ kind ++ "/" ++ String.fromInt rid) "" 0 (acceptLoad model ConflictLoaded) )

casePayload : State.CaseForm -> E.Value
casePayload cf = Types.encodeCaseInput { category=cf.category,destination=cf.destination,stage=cf.stage,deadline=cf.deadline,caseId=cf.caseId }

financeCandidateTag : Model -> String
financeCandidateTag model =
    String.join ":" [Maybe.withDefault "" model.token,Maybe.withDefault "" (Dict.get "id" model.featureForm),model.financeCandidateQuery,String.fromInt model.financeCandidateOffset]

loadFinanceCandidates : Model -> Cmd Msg
loadFinanceCandidates model =
    let
        ident = Dict.get "id" model.featureForm |> Maybe.withDefault ""
        ctx = {base=model.apiBase,token=model.token}
    in
    if ident == "" then Cmd.none else
        Api.featureGet ctx ("reconciliation/candidates/" ++ ident) model.financeCandidateQuery model.financeCandidateOffset (FinanceCandidatesLoaded (financeCandidateTag model))


contactFormOpen : String -> Model -> Bool
contactFormOpen target model =
    case target of
        "add-lead" -> model.showAddLead
        "edit-lead" -> model.editLeadOpen
        "student" -> model.showClientForm
        _ -> False


type alias ContactData =
    { payload : E.Value, email : String, phone : String, recordId : Maybe Int, kind : String }


contactData : String -> Model -> ContactData
contactData target model =
    case target of
        "add-lead" -> { payload=Types.encodeNewLead model.addLead, email=model.addLead.email, phone=model.addLead.phone, recordId=Nothing, kind="leads" }
        "edit-lead" -> { payload=Types.encodeUpdateLead model.editLead, email=model.editLead.email, phone=model.editLead.phone, recordId=Maybe.map (.lead >> .id) model.detail, kind="leads" }
        _ -> { payload=clientPayload model, email=model.clientForm.email, phone=model.clientForm.phone, recordId=model.clientEditingId, kind="clients" }


contactSignature : String -> Model -> String
contactSignature target model =
    let contact = contactData target model in
    target ++ ":" ++ Maybe.withDefault "new" (Maybe.map String.fromInt contact.recordId) ++ ":" ++ E.encode 0 contact.payload


checkContactBeforeSave : String -> Model -> ( Model, Cmd Msg )
checkContactBeforeSave target model =
    let
        contact = contactData target model
        ctx = { base = model.apiBase, token = model.token }
        body = E.object [("email",E.string contact.email),("phone",E.string contact.phone),("kind",E.string contact.kind),("exclude",Maybe.map E.int contact.recordId |> Maybe.withDefault E.null)]
    in
    ( { model | featureBusy = True }, Api.checkContact ctx body (acceptLoad model (ContactCheckDone target (contactSignature target model) contact.payload)) )


contactConfirmation : String -> Msg -> Html Msg
contactConfirmation message action =
    let
        matches = case action of
            ContactSaveConfirmed _ _ _ rows -> rows
            _ -> []
    in
    Ui.modal (if List.isEmpty matches then "Confirm this action" else "Possible duplicate")
        (div []
            [ Html.p [] [ text message ]
            , Html.ul [ class "duplicate-preview" ] (List.map (\item -> Html.li []
                [ a [ href (Features.str "path" item), Html.Attributes.target "_blank", Html.Attributes.rel "noopener" ] [text (Features.str "name" item ++ " · " ++ Features.str "number" item ++ " · " ++ Features.str "field" item ++ " match ↗")] ]) matches)
            , div [class "flex", Html.Attributes.style "margin-top" "20px"]
                [ button [class "btn btn-outline",onClick ConfirmDismissed] [text "Cancel"]
                , button [class "btn btn-primary",onClick ConfirmAccepted] [text (if List.isEmpty matches then "Confirm" else "Save as separate record")]
                ]
            ])
