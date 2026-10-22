module State exposing
    ( applicationFiltersForUrl
    , SessionDraft
    , Model
    , Msg(..)
    , Route(..)
    , Toast
    , AddLeadForm
    , EditLeadForm
    , AddTaskForm
    , ClientForm
    , CaseForm
    , UserForm
    , emptyAddLead
    , emptyEditLeadFrom
    , emptyAddTask
    , emptyClientForm
    , emptyCaseForm
    , emptyUserForm
    , caseFormOf
    , editLeadOf
    , clientFormOf
    , userFormOf
    , routeOf
    , initialModel
    )

{-| Central application state and messages.
-}

import Browser
import Time
import Browser.Navigation exposing (Key)
import File exposing (File)
import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Url exposing (Url)
import Url.Parser as P exposing ((</>), int, s, top)

import Types exposing (..)


-- ---------- routing ----------


type Route
    = Dashboard
    | LeadsPage
    | LeadDetailPage Int
    | ClientsPage
    | ClientDetailPage Int
    | TasksPage
    | UsersPage
    | AuditPage
    | WorkspacePage String


routeParser : P.Parser (Route -> a) a
routeParser =
    P.oneOf
        [ P.map (WorkspacePage "work/applications") (s "applications")
        , P.map (\section -> WorkspacePage ("work/" ++ section)) (s "work" </> P.string)
        , P.map (WorkspacePage "reminders") (s "reminders")
        , P.map (WorkspacePage "duplicates") (s "duplicates")
        , P.map (WorkspacePage "archive") (s "archive")
        , P.map (WorkspacePage "metrics") (s "system")
        , P.map Dashboard top
        , P.map LeadsPage (s "leads")
        , P.map LeadDetailPage (s "leads" </> int)
        , P.map ClientsPage (s "clients")
        , P.map ClientDetailPage (s "clients" </> int)
        , P.map TasksPage (s "tasks")
        , P.map UsersPage (s "users")
        , P.map AuditPage (s "audit")
        ]


routeOf : Url -> Route
routeOf url =
    Maybe.withDefault Dashboard (P.parse routeParser url)



-- ---------- forms & ui state ----------


type alias AddLeadForm =
    { fullName : String
    , email : String
    , phone : String
    , source : String
    , quality : String
    , stage : String
    , destination : String
    , service : String
    , notes : String
    , nextFollowup : String
    }


emptyAddLead : AddLeadForm
emptyAddLead =
    { fullName = ""
    , email = ""
    , phone = ""
    , source = "Website"
    , quality = "Warm"
    , stage = "New Lead"
    , destination = "Canada"
    , service = "Student Visa"
    , notes = ""
    , nextFollowup = ""
    }


type alias EditLeadForm =
    { fullName : String
    , email : String
    , phone : String
    , source : String
    , quality : String
    , stage : String
    , destination : String
    , service : String
    , notes : String
    , lastContact : String
    , nextFollowup : String
    , consultantId : Maybe Int
    , revision : Int
    }


emptyEditLeadFrom : EditLeadForm
emptyEditLeadFrom =
    { fullName = ""
    , email = ""
    , phone = ""
    , source = "Website"
    , quality = "Warm"
    , stage = "New Lead"
    , destination = "Canada"
    , service = "Student Visa"
    , notes = ""
    , lastContact = ""
    , nextFollowup = ""
    , consultantId = Nothing
    , revision = 1
    }


editLeadOf : Lead -> EditLeadForm
editLeadOf l =
    { fullName = l.fullName
    , email = Maybe.withDefault "" l.email
    , phone = Maybe.withDefault "" l.phone
    , source = Maybe.withDefault "Website" l.source
    , quality = Maybe.withDefault "Warm" l.quality
    , stage = l.stage
    , destination = Maybe.withDefault "Canada" l.destination
    , service = Maybe.withDefault "Student Visa" l.service
    , notes = Maybe.withDefault "" l.notes
    , lastContact = Maybe.withDefault "" l.lastContact
    , nextFollowup = Maybe.withDefault "" l.nextFollowup
    , consultantId = l.consultantId
    , revision = l.revision
    }


type alias AddTaskForm =
    { assignee : String
    , recurrenceDays : String
    , leadId : String
    , title : String
    , description : String
    , dueDate : String
    , priority : String
    }


emptyAddTask : AddTaskForm
emptyAddTask =
    { assignee = "", recurrenceDays = "", leadId = "", title = "", description = "", dueDate = "", priority = "Medium" }


type alias ClientForm =
    { fullName : String
    , email : String
    , phone : String
    , officerId : String
    , revision : Int
    }


emptyClientForm : ClientForm
emptyClientForm =
    { fullName = "", email = "", phone = "", officerId = "", revision = 1 }


clientFormOf : Client -> ClientForm
clientFormOf c =
    { fullName = c.fullName
    , email = Maybe.withDefault "" c.email
    , phone = Maybe.withDefault "" c.phone
    , revision = c.revision
    , officerId = Maybe.map String.fromInt c.caseOfficerId |> Maybe.withDefault ""
    }


type alias CaseForm =
    { caseId : Maybe Int
    , category : String
    , destination : String
    , stage : String
    , deadline : String
    , revision : Int
    }


{-| Create \/ edit form for the admin Users page.  @userId == Nothing@ means
"create"; on edit an empty password keeps the current one.
-}
type alias UserForm =
    { userId : Maybe Int
    , name : String
    , email : String
    , role : String
    , password : String
    , revision : Int
    }


emptyUserForm : UserForm
emptyUserForm =
    { userId = Nothing, name = "", email = "", role = "consultant", password = "", revision = 1 }


userFormOf : User -> UserForm
userFormOf u =
    { revision = u.revision
    , userId = Just u.id
    , name = u.name
    , email = u.email
    , role = u.role
    , password = ""
    }


emptyCaseForm : CaseForm
emptyCaseForm =
    { caseId = Nothing, category = "Student Visa", destination = "Canada"
    , stage = "Documents Under Review", deadline = "", revision = 1
    }


caseFormOf : CaseRow -> CaseForm
caseFormOf cr =
    { caseId = Just cr.id
    , category = Maybe.withDefault "Student Visa" cr.category
    , destination = Maybe.withDefault "Canada" cr.destination
    , stage = cr.stage
    , revision = cr.revision
    , deadline = Maybe.withDefault "" cr.deadline
    }


type alias Toast =
    { id : Int, kind : String, msg : String }



-- ---------- model ----------


type alias SessionDraft =
    { path : String
    , ownerId : Int
    , addLead : AddLeadForm
    , editLead : EditLeadForm
    , addTask : AddTaskForm
    , clientForm : ClientForm
    , caseForm : Maybe CaseForm
    , noteDraft : String
    , featureForm : Dict String String
    , featureBaseline : Dict String String
    , recordBaseline : D.Value
    , inlineSection : String
    , inlineOwner : String
    , formDirty : Bool
    , editingTask : Bool
    , showAddLead : Bool
    , editLeadOpen : Bool
    , showGlobalAddTask : Bool
    , showClientForm : Bool
    , clientEditingId : Maybe Int
    }


type alias Model =
    { key : Key
    , url : Url
    , apiBase : String
    , token : Maybe String
    , user : Maybe User
    , sessionDraft : Maybe SessionDraft
    , authChecked : Bool
    , darkMode : Bool

    -- login form
    , confirmation : Maybe ( String, Msg )
    , loginMode : String
    , loginBusy : Bool
    , loginNotice : String
    , userEpoch : Int
    , loadEpoch : Int
    , pageOffset : Int
    , pageLoading : Bool
    , pageError : String
    , opsData : List D.Value
    , loginEmail : String
    , loginOtp : String
    , featureData : D.Value
    , featureResult : D.Value
    , reportSettings : Dict String String
    , featureForm : Dict String String
    , inlineSection : String
    , inlineOwner : String
    , featureBaseline : Dict String String
    , featureBusy : Bool
    , featureErrors : Dict String String
    , formDirty : Bool
    , portalBusy : Bool
    , portalNotice : String
    , editingTask : Bool
    , documentBusy : Bool
    , clientProfile : D.Value
    , clientPhotoPreview : Maybe String
    , clientPhotoFile : Maybe File
    , photoEpoch : Int
    , clientPhotoBusy : Bool
    , clientPhotoNotice : String
    , clientPhotoError : String
    , featureError : String
    , financeFile : Maybe File
    , financeCandidates : List E.Value
    , financeCandidateQuery : String
    , financeCandidateOffset : Int
    , checklistFile : Maybe File
    , conflictCurrent : D.Value
    , conflictKind : String
    , conflictOriginal : Dict String String
    , conflictEdits : Dict String String
    , recordBaseline : D.Value
    , applicationFilters : Dict String String
    , featureSearch : String
    , portalData : D.Value
    , portalError : String
    , portalFile : Maybe File
    , loginPassword : String
    , loginError : String

    -- data caches
    , stats : Maybe Stats
    , leads : List Lead
    , users : List User
    , tasks : List Task
    , clients : List Client
    , clientDetail : Maybe ClientDetail
    , detail : Maybe LeadDetail

    -- leads filters / selection
    , q : String
    , fStage : String
    , fQuality : String
    , fSource : String
    , sortBy : String
    , sortAsc : Bool
    , selected : Set Int

    -- modals & forms
    , showAddLead : Bool
    , addLead : AddLeadForm
    , editLeadOpen : Bool
    , editLead : EditLeadForm
    , noteDraft : String
    , addTask : AddTaskForm
    , showGlobalAddTask : Bool

    , clientQ : String
    , showClientForm : Bool
    , clientEditingId : Maybe Int
    , clientForm : ClientForm

    , caseForm : Maybe CaseForm

    -- users / account
    , showUserForm : Bool
    , userForm : UserForm
    , showPwModal : Bool
    , pwCurrent : String
    , pwNew : String
    , pwConfirm : String

    -- audit trail
    , audit : List AuditEntry

    -- document upload
    , pendingDoc : Maybe File

    -- csv import (leads)
    , pendingImport : Maybe File

    -- chrome
    , notifOpen : Bool
    , toasts : List Toast
    , toastSeq : Int
    , currentTime : String
    , clockTimezone : String
    , clockOffsetMinutes : Int
    , clockSkewMillis : Int
    , clockSynced : Bool
    , today : String
    }


initialModel : Key -> Url -> Model
initialModel key url =
    { key = key
    , url = url
    , apiBase = ""
    , token = Nothing
    , user = Nothing
    , sessionDraft = Nothing
    , authChecked = False
    , darkMode = False
    , confirmation = Nothing
    , loginMode = if Maybe.withDefault "" url.query |> String.startsWith "reset=" then "reset" else "login"
    , loginBusy = False
    , loginNotice = ""
    , userEpoch = 0
    , loadEpoch = 0
    , pageOffset = 0
    , pageLoading = True
    , pageError = ""
    , opsData = []
    , loginEmail = ""
    , loginOtp = ""
    , featureData = E.null
    , featureResult = E.null
    , reportSettings = Dict.empty
    , featureForm = Dict.empty
    , inlineSection = "tasks"
    , inlineOwner = case routeOf url of
        ClientDetailPage cid -> "client:" ++ String.fromInt cid
        _ -> ""
    , featureBaseline = Dict.empty
    , featureBusy = False
    , featureErrors = Dict.empty
    , formDirty = False
    , portalBusy = url.path == "/portal"
    , portalNotice = ""
    , editingTask = False
    , documentBusy = False
    , clientProfile = E.null
    , clientPhotoPreview = Nothing
    , clientPhotoFile = Nothing
    , photoEpoch = 0
    , clientPhotoBusy = False
    , clientPhotoNotice = ""
    , clientPhotoError = ""
    , featureError = ""
    , financeFile = Nothing
    , financeCandidates = []
    , financeCandidateQuery = ""
    , financeCandidateOffset = 0
    , checklistFile = Nothing
    , conflictCurrent = E.null
    , conflictKind = ""
    , conflictOriginal = Dict.empty
    , conflictEdits = Dict.empty
    , recordBaseline = E.null
    , applicationFilters = applicationFiltersForUrl url
    , featureSearch = ""
    , portalData = E.null
    , portalError = ""
    , portalFile = Nothing
    , loginPassword = ""
    , loginError = ""

    , stats = Nothing
    , leads = []
    , users = []
    , tasks = []
    , clients = []
    , clientDetail = Nothing
    , detail = Nothing

    , q = ""
    , fStage = "All"
    , fQuality = "All"
    , fSource = "All"
    , sortBy = "name"
    , sortAsc = True
    , selected = Set.empty

    , showAddLead = False
    , addLead = emptyAddLead
    , editLeadOpen = False
    , editLead = emptyEditLeadFrom
    , noteDraft = ""
    , addTask = emptyAddTask
    , showGlobalAddTask = False

    , clientQ = ""
    , showClientForm = False
    , clientEditingId = Nothing
    , clientForm = emptyClientForm

    , caseForm = Nothing

    , showUserForm = False
    , userForm = emptyUserForm
    , showPwModal = False
    , pwCurrent = ""
    , pwNew = ""
    , pwConfirm = ""

    , audit = []

    , pendingDoc = Nothing

    , pendingImport = Nothing

    , notifOpen = False
    , toasts = []
    , toastSeq = 0
    , currentTime = ""
    , clockTimezone = "Asia/Manila"
    , clockOffsetMinutes = 480
    , clockSkewMillis = 0
    , clockSynced = False
    , today = ""
    }



-- ---------- messages ----------


type Msg
    = AcceptLoad String Msg
    | ConfirmRequested String Msg
    | ConfirmAccepted
    | ConfirmDismissed
    | LoginModeChanged String
    | RecoverClicked
    | ResetClicked
    | RecoveryDone (Result Http.Error ())
    | ResetDone (Result Http.Error ())
    | LogoutDone (Result Http.Error ())
    | PageStep Int
    | SearchReady String String
    | OperationsLoaded (Result Http.Error (List D.Value))
    | RunOperation String
    | OperationDone (Result Http.Error ())
    | FeatureLoaded (Result Http.Error D.Value)
    | InlineSection String
    | InlineOwner String
    | FinanceFile File
    | FinanceUpload Int Int
    | FinanceCandidatesLoaded String (Result Http.Error E.Value)
    | FinanceCandidateQuery String
    | FinanceCandidateStep Int
    | FinanceCandidateSelected E.Value
    | ChecklistFile File
    | ChecklistUpload Int Int
    | ConflictLoaded (Result Http.Error D.Value)
    | ConflictRebase
    | ConflictDismiss
    | ConflictUseCurrent String
    | ApplicationFilter String String
    | ClearApplicationFilters
    | ReportSetting String String
    | ReportOutput String
    | ReportSettingsLoaded D.Value
    | FeatureField String String
    | FeatureSearch String
    | FeatureEdit D.Value
    | DiscardChanges Msg
    | EditTask Task
    | TaskEditorLoaded (Result Http.Error D.Value)
    | CloseTaskEditor
    | FeatureReset
    | FeatureRun String
    | FeatureDone (Result Http.Error D.Value)
    | LoginOtpChanged String
    | PortalExchange String
    | PortalExchanged (Result Http.Error D.Value)
    | PortalLoaded (Result Http.Error D.Value)
    | PortalFile File
    | PortalUpload Int
    | PortalUploaded (Result Http.Error D.Value)
    | PortalRefresh
    | PortalLogout
    | PortalLogoutDone (Result Http.Error D.Value)
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotStoredToken (Maybe String)
    | GotStoredDark Bool
    | ToggleDarkMode
      -- auth
    | LoginEmailChanged String
    | LoginPasswordChanged String
    | LoginClicked
    | LoginDone (Result Http.Error LoginResponse)
    | MeDone (Result Http.Error User)
    | LogoutClicked
      -- data loads
    | LeadsLoaded (Result Http.Error (List Lead))
    | UsersLoaded (Result Http.Error (List User))
    | TasksLoaded (Result Http.Error (List Task))
    | ClientsLoaded (Result Http.Error (List Client))
    | ClientDetailLoaded (Result Http.Error ClientDetail)
    | DetailLoaded (Result Http.Error LeadDetail)
    | StatsLoaded (Result Http.Error Stats)
    | ClockStatsLoaded String (Result Http.Error Stats)
    | AuditLoaded (Result Http.Error (List AuditEntry))
      -- lead CRUD
    | OpenAddLead
    | CloseAddLead
    | UpdateAddLead (AddLeadForm -> AddLeadForm)
    | SubmitAddLead
    | ContactCheckDone String String E.Value (Result Http.Error D.Value)
    | ContactSaveConfirmed String String E.Value (List D.Value)
    | AddLeadDone (Result Http.Error Lead)
    | EditLeadToggled
    | UpdateEditLead (EditLeadForm -> EditLeadForm)
    | SubmitEditLead
    | EditLeadDone (Result Http.Error Lead)
    | DeleteLeadConfirmed Int
    | DeleteLeadDone (Result Http.Error ())
    | AdvanceClicked Int
    | AdvanceDone (Result Http.Error Lead)
    | ClientProfileLoaded (Result Http.Error D.Value)
    | ClientPhotoPicked File
    | ClientPhotoPreviewReady File String
    | ClientPhotoUpload Int
    | ClientPhotoDone Int (Result Http.Error D.Value)
    | ClientProfileRefresh Int
    | ClientPhotoRetry Int
    | ConvertClicked Int
    | ConvertDone (Result Http.Error Client)
    | NoteDraftChanged String
    | NoteSubmitted Int
    | NoteDone (Result Http.Error ())
      -- filters / bulk
    | SearchChanged String
    | StageFilterChanged String
    | QualityFilterChanged String
    | SourceFilterChanged String
    | SortByChanged String
    | SortDirToggled
    | ClearFilters
    | RowChecked Int Bool
    | SelectAllChecked Bool
    | BulkAdvanceClicked
    | BulkAssignPicked String
    | BulkDeleteClicked
    | BulkDone (Result Http.Error ())
      -- tasks
    | TaskStatusPicked Int String
    | TaskStatusDone (Result Http.Error ())
    | TaskDeleted Int
    | TaskDeleteDone (Result Http.Error ())
    | ToggleGlobalAddTask
    | UpdateAddTask (AddTaskForm -> AddTaskForm)
    | SubmitAddTaskFor Int
    | SubmitClientTask Int
    | SubmitAddTaskGlobal
    | AddTaskDone (Result Http.Error Task)
      -- clients
    | ClientSearchChanged String
    | OpenAddClient
    | CloseClientForm
    | UpdateClientForm (ClientForm -> ClientForm)
    | SubmitClientForm
    | ClientFormSaved (Result Http.Error Client)
    | ClientUpdated (Result Http.Error ())
    | DeleteClientConfirmed Int
    | DeleteClientDone (Result Http.Error ())
    | OpenCaseForm (Maybe CaseRow)
    | CloseCaseForm
    | UpdateCaseForm (CaseForm -> CaseForm)
    | SubmitCaseForm Int
    | CaseFormSaved (Result Http.Error ClientDetail)
    | DeleteCaseClicked Int
    | DeleteCaseDone (Result Http.Error ())
      -- documents
    | DocPicked File
    | DocCleared
    | UploadLeadDocClicked Int
    | UploadClientDocClicked Int
    | LeadDocUploaded (Result Http.Error LeadDetail)
    | ClientDocUploaded (Result Http.Error ClientDetail)
    | DocDeleted Int
    | DocDeleteDone (Result Http.Error ())
      -- csv import (leads)
    | ImportFilePicked File
    | LeadImportClicked
    | LeadsImported (Result Http.Error ImportResult)
      -- users / account
    | OpenAddUser
    | OpenUserEditor Int
    | CloseUserForm
    | UpdateUserForm (UserForm -> UserForm)
    | SubmitUserForm
    | UserSaved (Result Http.Error User)
    | UserUpdatedDone (Result Http.Error ())
    | DeleteUserConfirmed Int
    | DeleteUserDone (Result Http.Error ())
    | TogglePwModal
    | PwCurrentChanged String
    | PwNewChanged String
    | PwConfirmChanged String
    | SubmitPasswordChange
    | PasswordChangeDone (Result Http.Error ())
      -- chrome
    | NotifBellClicked
    | NavigateTo String
    | TodayChanged String
    | ClockTick Time.Posix
    | ClockRefresh Time.Posix
    | ClockRefreshRequested
    | ClockLoaded (Result Http.Error D.Value)
    | ClockAligned Int Int String Time.Posix
    | OpenClientEditor Int
    | ToastPushed String String
    | ToastExpired Int


applicationFiltersForUrl : Url -> Dict String String
applicationFiltersForUrl url =
    Maybe.withDefault "" url.query |> String.split "&"
        |> List.filterMap (\part -> case String.split "=" part of
            [key,value] -> if List.member key ["client","officer","destination","stage","intake"] then Just(key,Url.percentDecode value |> Maybe.withDefault "") else Nothing
            _ -> Nothing)
        |> Dict.fromList
