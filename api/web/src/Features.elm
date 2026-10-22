module Features exposing (clientNextSteps, clientPhotoCard, heading, str, view, embedded, conflictView, formValues, portalView, rowForm, defaults)

import Char
import Dict exposing (Dict)
import File
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import State exposing (..)
import Ui
import Url
import Reports
import Validation

str : String -> D.Value -> String
str key item = D.decodeValue (D.field key scalar) item |> Result.withDefault ""

scalar : D.Decoder String
scalar = D.oneOf [ D.string, D.map String.fromInt D.int, D.map String.fromFloat D.float, D.map (\b -> if b then "true" else "false") D.bool, D.null "" ]

items : String -> D.Value -> List D.Value
items key item = D.decodeValue (D.field key (D.list D.value)) item |> Result.withDefault []

formValues : D.Value -> Dict String String
formValues row =
    D.decodeValue (D.keyValuePairs D.value) row |> Result.withDefault []
        |> List.filterMap (\(key,value) -> D.decodeValue scalar value |> Result.toMaybe |> Maybe.map (Tuple.pair key))
        |> Dict.fromList
        |> Dict.map (\key value_ -> if List.member key ["required","enabled","assignments","deadlines","documents","escalations","emailEnabled"] then (if value_=="1" then "true" else if value_=="0" then "false" else value_) else value_)

rowForm : D.Value -> Dict String String
rowForm row =
    let
        values = formValues row
        rid = str "id" row
    in
    values |> Dict.insert "expectedRevision" (str "revision" row) |> Dict.insert "request" ("request:" ++ rid) |> Dict.insert "session" rid |> Dict.insert "user" rid


defaults : String -> Dict String String
defaults section = Dict.fromList
    [ ("status", case section of
        "applications" -> "Pending"
        "milestones" -> "Planned"
        "documents" -> "Missing"
        "appointments" -> "Scheduled"
        "invoices" -> "Draft"
        _ -> "Pending")
    , ("expectedRevision","0"), ("kind","Offer"), ("offerStatus","Not requested"), ("enrollmentStatus","Not enrolled"), ("channel","Call"), ("direction","Outgoing"), ("outcome","Completed")
    , ("category","Student Visa"), ("destination","Canada"), ("stage","Documents Under Review")
    , ("required","true"), ("enabled","true"), ("priority","Medium")
    , ("entity", if section == "merge" then "leads" else "tasks"), ("currency","PHP"), ("daysBefore","1"), ("escalateAfter","2"), ("days","7")
    , ("mine","true"), ("overdue","false"), ("assignments","true"), ("deadlines","true"), ("documents","true"), ("escalations","true"), ("emailEnabled","true")
    ]

-- Fields carry labels and control types; internal API field names are never displayed.
type alias Field = ( String, String, String )

fields : String -> List Field
fields section = case section of
    "sales" -> [("From date","from","date"),("To date","to","date")]
    "cashflow" -> [("From date","from","date"),("To date","to","date")]
    "balances" -> []
    "finance-audit" -> []
    "payment-history" -> [("Reversal date","paidOn","date"),("Reason for reversal","reason","textarea")]
    "periods" -> [("Financial month","month","month"),("Reason for closing or reopening","reason","textarea")]
    "reconciliation" -> [("Bank account label","account","text"),("Currency","currency","PHP|USD|EUR|GBP|AUD|CAD|NZD"),("Statement CSV (transactionId,date,amount,reference,description)","csv","textarea"),("Matching or exclusion reason","reason","textarea")]
    "payments" -> [("Payment amount","amount","text"),("Payment / reversal date","paidOn","date"),("Payment method","method","Cash|Bank transfer|Card|Cheque|Other"),("Payment reference","reference","text"),("Reason for reversal","reason","textarea")]
    "expenses" -> [("Supplier / payee","supplier","text"),("Description","description","textarea"),("Expense category","category","text"),("Currency","currency","PHP|USD|EUR|GBP|AUD|CAD|NZD"),("Amount","amount","text"),("Expense date","expenseOn","date"),("Invoice / supplier reference","reference","text"),("Payment / reversal date","paidOn","date"),("Payment method","method","Cash|Bank transfer|Card|Cheque|Other"),("Payment reference","paymentReference","text"),("Review or reversal reason","reason","textarea")]
    "applications" -> [("Student","owner","client"),("Service / application type","category","Student Visa|Work Visa|Tourist Visa|Immigration Consultation|School Admission"),("Destination","destination","Canada|Australia|New Zealand|UAE|UK|Schengen"),("Stage","stage","Documents Under Review|Submitted|Decision Pending|Approved|Rejected"),("School / institution","institution","text"),("Course / program","program","text"),("Offer status","offerStatus","Not requested|Requested|Conditional offer|Unconditional offer|Declined"),("Enrollment","enrollmentStatus","Not enrolled|Pending|Confirmed|Deferred|Withdrawn"),("Intake date","intakeDate","date"),("Next deadline","deadline","date"),("Checklist template","template","template")]
    "checklist-templates" -> [("Checklist name","name","text"),("Application type","category","Any|Student Visa|Work Visa|Tourist Visa|Immigration Consultation|School Admission"),("Destination (blank for all)","destination","text"),("Required documents (one per line)","requiredDocuments","textarea"),("Optional documents (one per line)","optionalDocuments","textarea")]
    "tasks" -> [("Title","title","text"),("Description","description","textarea"),("Due date","dueDate","date"),("Priority","priority","Low|Medium|High|Urgent"),("Status","status","Pending|In Progress|Done"),("Assigned to","assignee","staff"),("Repeat every (days; blank for one time)","recurrenceDays","number")]
    "milestones" -> [("Application","owner","case"),("Milestone type","kind","Offer|Deposit|Visa submission|Visa decision|Arrival|Other"),("Milestone","title","text"),("Planned date","plannedOn","date"),("Completed date","completedOn","date"),("Status","status","Planned|In progress|Completed|Cancelled"),("Notes","note","textarea")]
    "contact-log" -> [("Student or lead","owner","record"),("Channel","channel","Call|WhatsApp|SMS|Email|Meeting"),("Direction","direction","Incoming|Outgoing"),("Outcome","outcome","Completed|No answer|Follow-up needed"),("Interaction date","occurredOn","date"),("Conversation and next action","note","textarea")]
    "history" -> [("Record","owner","record"),("Note or conversation summary","note","textarea")]
    "documents" -> [("Record","owner","record"),("Required document","title","text"),("Required?","required","true|false"),("Review status","status","Missing|Received|Approved|Rejected"),("Expires on","expiresOn","date"),("Feedback for the student","reviewNote","textarea"),("Existing file to attach as a new version","document","document")]
    "reminders" -> [("Task","task","task"),("Days before deadline","daysBefore","number"),("Escalate to administrators after overdue days","escalateAfter","number"),("Email reminders enabled?","enabled","true|false")]
    "merge" -> [("Record type","entity","leads|clients"),("Source to archive","source","merge"),("Surviving record","target","merge"),("Type MERGE after reviewing the preview","confirmation","text")]
    "reports" -> [("From date","from","date"),("To date","to","date")]
    "search" -> [("View name","name","text"),("Records","entity","leads|clients|tasks|cases"),("Contains","query","text"),("Assigned to me?","mine","true|false"),("Overdue only?","overdue","true|false")]
    "security" -> [("Your current password","password","password"),("Your authenticator or recovery code","code","text")] ++ [("Reason for resetting another account’s authenticator","reason","textarea")]
    "appointments" -> [("Record","owner","record"),("Appointment","title","text"),("Staff member","assignee","staff"),("Starts (UTC)","startsAt","datetime-local"),("Ends (UTC)","endsAt","datetime-local"),("Location or meeting link","location","text"),("Status","status","Scheduled|Completed|Cancelled")]
    "invoices" -> [("Student","owner","client"),("Description","description","textarea"),("Currency","currency","PHP|USD|EUR|GBP|AUD|CAD|NZD"),("Amount","amount","text"),("Due date","dueDate","date"),("Status","status","Draft|Issued|Paid|Void")]
    "portal" -> [("Student","owner","client"),("Access duration (days)","days","number"),("Approved file to share","document","document")]
    "notifications" -> [("Task assignments","assignments","true|false"),("Upcoming deadlines","deadlines","true|false"),("Document rejections","documents","true|false"),("Overdue escalation","escalations","true|false"),("Reminder emails","emailEnabled","true|false")]
    "mail" -> []
    "ageing" -> [("Created from","from","date"),("Created to","to","date")]
    "bottlenecks" -> [("Created from","from","date"),("Created to","to","date")]
    "trends" -> [("From","from","date"),("To","to","date")]
    "communication" -> [("Contact record","owner","record"),("Subject","subject","text"),("Message","message","textarea")]
    _ -> []

heading : String -> (String,String)
heading section = case section of
    "sales" -> ("Sales report","Lead cohorts by source and consultant, conversion rates and pipeline stages. Archived records remain in financial history; merged duplicates are excluded. Invoiced and net receipts use their own posting dates; balances are current. Each currency is shown separately.")
    "payment-history" -> ("Searchable payment history","Search receipts by student, reference, recorder or receipt number. Every entry remains available through pagination. Select an unreversed receipt to correct it, or print its acknowledgement.")
    "periods" -> ("Financial period closing","Close completed months after bank reconciliation. Closed months reject backdated financial entries. Reopening requires an authorized user with company-wide access and an audit reason. Saved closing totals appear below.")
    "reconciliation" -> ("Bank statement reconciliation","Paste a statement CSV with transactionId,date,amount,reference,description columns. Credits are positive, debits negative. Select a bank entry, choose a matching recorded payment below and enter a reason. Exclude non-CRM entries explicitly with a reason.")
    "payments" -> ("Payments and history","Select an invoice below to record a full or partial payment. Overpayments are blocked. Open Payment history to reverse an incorrect receipt with a date and reason; its original entry remains visible. These records do not charge a card or move money.")
    "balances" -> ("Balances and overdue accounts","Current invoice totals, net payments and outstanding balances. Overdue amounts are unpaid balances past the due date. Use Payments to record receipts.")
    "expenses" -> ("Expenses and approvals","Submit an expense, select it for approval, then record its payment. Non-admin approvers cannot approve their own expenses. Posted payments are corrected through reversals. Clear the form to submit a new expense.")
    "cashflow" -> ("Receipts and expenses","Net payment entries received minus visible paid expenses for the selected period, grouped by currency. Company approvers see all expenses; other users see their own. Pending and approved-but-unpaid expenses are excluded. This is a cash activity report, not a profit or tax statement.")
    "finance-audit" -> ("Financial audit trail","Payment receipts, expense reviews and reversals retain their dates, references, reasons and the person who recorded them.")
    "applications" -> ("Applications","Each application belongs to a student. A student can have multiple applications. Create one here, or select an application below to update its stage and deadline.")
    "checklist-templates" -> ("Document checklist templates","Administrators maintain reusable document lists. Apply a matching template from a selected application. Existing requirements and reviews are preserved when a checklist is applied again.")
    "tasks" -> ("Edit and assign tasks","Select a task below to edit its deadline, owner and recurrence. A completed recurring task creates its next occurrence automatically.")
    "milestones" -> ("Application milestones","Track offers, deposits, visa submissions, decisions and arrival dates. A deposit milestone is a planning record; use Payments to record money received.")
    "contact-log" -> ("Calls and messages","Log conversations and follow-ups with a student. WhatsApp and SMS are handled in your usual phone apps; saving this log does not send a message.")
    "history" -> ("Student and application history","Keep conversations, decisions and next steps together. Notes are attributed to their author.")
    "documents" -> ("Document checklist and review","Create requirements, upload directly into an item, and record approval or feedback once scanning succeeds.")
    "reminders" -> ("Email reminders and escalation","Send deadline reminders to the assigned staff member and escalate overdue work to administrators.")
    "merge" -> ("Merge duplicate records","Preview both records before confirming. Children are preserved; the source is archived and its original fields remain available in the audit trail.")
    "reports" -> ("Management reports","Choose a date range. Lead conversions use the cohort created in that range; workload and overdue counts show the current position.")
    "search" -> ("Advanced search","Search contacts, phone numbers, tasks and cases. Save personal views for work such as your overdue cases.")
    "security" -> ("Account security","Set up a SHA-256 compatible authenticator, keep recovery codes offline, and review or revoke active sessions.")
    "appointments" -> ("Appointments and calendar","Schedule appointments in UTC. Overlapping appointments for the same staff member are prevented.")
    "invoices" -> ("Invoices","Create drafts and issue invoices. Issued amounts are locked. Record full or partial receipts in Payments; Paid status is calculated from the payment history.")
    "portal" -> ("Student portal access","Create a private, one-use sign-in link. Only explicitly shared, approved files become downloadable by the student.")
    "notifications" -> ("Notification centre","Review assignments, deadlines, document rejections and overdue work. Preferences hide selected categories and control reminder emails.")
    "mail" -> ("Email delivery","Track queue status and failures. Accepted by SMTP means the mail server accepted the message; inbox delivery is not confirmed.")
    "ageing" -> ("Application ageing","Review open cases, their age and overdue deadlines. Exports contain up to 10,000 rows; narrow the date range for larger datasets.")
    "bottlenecks" -> ("Application bottlenecks","Applications grouped by stage, with time in the current stage. Older applications without stage history use their creation date.")
    "trends" -> ("Historical workload","Daily snapshots begin when the scheduler is enabled. Today updates during the day; previous days retain their last recorded workload.")
    "communication" -> ("Email a contact","Send through the configured mail service. Delivery is queued and the conversation is recorded in the shared history.")
    _ -> ("Workspace","")

view : Model -> String -> Html Msg
view model section =
    let
        (title,description) = heading section
        get key = Dict.get key model.featureForm |> Maybe.withDefault ""
        options = items "options" model.featureData
        isViewer = Maybe.map .role model.user == Just "viewer"
        canWrite = (not isViewer || List.member section ["security","search","notifications","sales","cashflow","balances","finance-audit"]) && (section /= "checklist-templates" || Maybe.map .role model.user == Just "admin")
        control (labelText,key,kind) =
            let
                selectedValue = get key
                choices = if String.contains "|" kind then String.split "|" kind |> List.map (\v -> (v, if v == "true" then "Yes" else if v == "false" then "No" else if v == "all" then "All workspace records" else if v == "assigned" then "Assigned records" else if v == "allowed" then "Allowed" else if v == "denied" then "Denied" else if v == "clients" then "Students" else v)) else
                    options |> List.filter (\item -> let v=str "value" item in case kind of
                        "record" -> List.any (\prefix -> String.startsWith prefix v) ["lead:","client:","case:"]
                        "merge" -> String.startsWith (if get "entity" == "clients" then "client:" else "lead:") v
                        _ -> String.startsWith (kind ++ ":") v)
                    |> List.map (\item -> let v=str "value" item in (if List.member key ["assignee","task","source","target","template","payment"] then String.split ":" v |> List.reverse |> List.head |> Maybe.withDefault v else v,str "label" item))
                fieldError = Dict.get key model.featureErrors |> Maybe.withDefault ""
                attrs = [ class "input", value selectedValue, onInput (FeatureField key), id ("work-" ++ key), disabled (not canWrite || model.featureBusy || (section == "expenses" && get "id" /= "" && List.member key ["supplier","description","category","currency","amount","expenseOn","reference"]) || (section == "applications" && key == "owner" && get "id" /= "")), attribute "aria-invalid" (if fieldError == "" then "false" else "true"), attribute "aria-describedby" ("error-" ++ key) ]
            in
            div [ class "form-group" ]
                [ label [ for ("work-" ++ key) ] [ text labelText ]
                , if kind == "textarea" then textarea (attrs ++ [rows 4]) []
                  else if List.member kind ["record","case","staff","task","client","document","merge","template","payment"] || String.contains "|" kind then select attrs
                    (option [value ""] [text "Choose…"] :: List.map (\(v,l) -> option [value v,selected (v==selectedValue)] [text l]) choices)
                  else input (attrs ++ [ type_ kind ]) []
                , p [ id ("error-" ++ key), class "field-error", attribute "role" "status" ] [ text fieldError ]
                ]
        action name labelText =
            if not canWrite || (List.member section ["payments","expenses","payment-history","periods","reconciliation"] && D.decodeValue (D.at ["permissions",name] D.bool) model.featureData /= Ok True) then text "" else
            button [ class "btn btn-primary", disabled model.featureBusy, onClick (if List.member name ["close","reopen","match","unmatch","ignore","import","record","reverse","submit","approve","reject","pay","reverse-expense","confirm","suspend","revoke-user","reset-mfa","access","grant","revoke","retry"] then ConfirmRequested ("Confirm: " ++ labelText ++ "?") (FeatureRun name) else FeatureRun name) ] [ text (if model.featureBusy then "Working…" else labelText) ]
        actions = case section of
            "sales" -> [action "range" "Update report"]
            "cashflow" -> [action "range" "Update report"]
            "balances" -> []
            "finance-audit" -> []
            "payments" -> [action "record" "Record payment"]
            "payment-history" -> if List.member (get "reversed") ["1","true"] || get "reversesId" /= "" then [] else [action "reverse" "Reverse selected receipt"]
            "periods" -> [action "close" "Close month",action "reopen" "Reopen month"]
            "reconciliation" -> if get "id" == "" then [action "import" "Import bank statement"] else [action "match" "Confirm selected match",action "unmatch" "Unmatch transaction",action "ignore" "Exclude transaction with reason"]
            "expenses" -> (if get "id" == "" then [action "submit" "Submit expense"] else [action "approve" "Approve expense",action "reject" "Reject expense",action "pay" "Record expense payment",action "reverse-expense" "Reverse expense payment"])
            "applications" -> [action "save" (if get "id" == "" then "Create application" else "Save application"),action "checklist" "Apply selected checklist"]
            "tasks" -> (if model.inlineOwner/="" then [action "create" "Create task here"] else []) ++ [action "save" "Save selected task"]
            "history" -> [action "add" "Add note"]
            "contact-log" -> [action "add" "Log interaction"]
            "milestones" -> [action "save" (if get "id" == "" then "Add milestone" else "Save milestone")]
            "merge" -> [action "preview" "Preview merge",action "confirm" "Confirm merge"]
            "reports" -> [action "range" "Update report"]
            "ageing" -> [action "range" "Update report"]
            "bottlenecks" -> [action "range" "Update report"]
            "trends" -> [action "range" "Update report"]
            "mail" -> if Maybe.map .role model.user==Just "admin" then [action "retry" "Retry selected failed message"] else []
            "notifications" -> [action "preferences" "Save notification preferences",action "read" "Mark selected notification read"]
            "search" -> [action "views" "Show saved views",action "save" "Save personal view",action "apply" "Open selected view",action "delete" "Delete selected view"]
            "security" -> [action "setup" "Start authenticator setup",action "enable" "Enable MFA",action "disable" "Disable MFA",action "revoke" "Revoke selected session"]
            "documents" -> [action "save" "Save checklist item"] ++ (if model.inlineOwner=="" then [action "attach" "Attach selected file as a new version"] else []) ++ (if Maybe.map .role model.user==Just "admin" then [action "rescan" "Retry failed scan for selected item"] else [])
            "portal" -> [action "create" "Create private sign-in link",action "grant" "Share selected approved file",action "revoke" "Revoke selected link"]
            "communication" -> [action "send" "Queue email"]
            "invoices" -> [action "save" "Save invoice",action "send" "Email selected invoice"]
            _ -> [action "save" "Save changes"]
        row item = div [class "operation-row"]
            [ div []
                [ h3 [] [text (str "title" item)]
                , if section=="documents" then p [class "status-help"] [text (scanHelp (str "scanStatus" item))] else if section=="mail" then p [class "status-help"] [text (mailHelp (str "status" item))] else text ""
                , if List.member section ["payments","expenses"] then div [] (List.map (\entry -> p [class "history-entry"] [text ("Entry #" ++ str "id" entry ++ " · " ++ str "amount" entry ++ " · " ++ (if str "date" entry == "" then "Undated legacy payment" else str "date" entry) ++ " · " ++ str "method" entry ++ " · " ++ str "reference" entry ++ " · " ++ str "actor" entry ++ " · " ++ str "reason" entry ++ (if str "reversesId" entry == "" then "" else " · Reverses #" ++ str "reversesId" entry))]) (items "history" item)) else text ""
                , if List.member section ["payments","balances"] then div [class "form-actions"] [a [class "btn btn-outline",href (str "historyUrl" item)] [text "Payment history"], if D.decodeValue (D.at ["permissions","export"] D.bool) model.featureData == Ok True then a [class "btn btn-outline",href (str "printUrl" item),target "_blank",rel "noopener"] [text "Print invoice"] else text ""] else text ""
                , if section=="payment-history" then div [] [if List.member (str "duplicateReference" item) ["1","true"] then p [class "notice-banner"] [text "This reference is used by more than one receipt. Verify it against the bank statement."] else text "", if D.decodeValue (D.at ["permissions","export"] D.bool) model.featureData == Ok True then a [class "btn btn-outline",href (str "printUrl" item),target "_blank",rel "noopener"] [text "Print acknowledgement"] else text ""] else text ""
                , if section=="expenses" then div [] (List.map (\file -> p [] [text (str "filename" file ++ " · " ++ str "scanStatus" file), if str "scanStatus" file == "Clean" && D.decodeValue (D.at ["permissions","download"] D.bool) model.featureData == Ok True then a [href ("/api/finance/attachments/" ++ str "id" file),class "btn btn-outline"] [text "Download supporting file"] else if str "scanStatus" file == "Error" && Maybe.map .role model.user == Just "admin" then button [class "btn btn-outline",onClick (FeatureRun ("rescan-file:" ++ str "id" file))] [text "Retry scan"] else text ""]) (items "files" item)) else text ""
                , if section=="periods" then div [] (List.map (\summary -> p [] [text (str "currency" summary ++ " · Invoiced " ++ str "invoiced" summary ++ " · Receipts " ++ str "receipts" summary ++ " · Paid expenses " ++ str "expenses" summary)]) (items "snapshot" item)) else text ""
                , if section=="reports" then reportRow item else p [] [text (String.join " · " (List.filter ((/=) "") (List.map (\key -> let v=str key item in if v=="" then "" else fieldLabel key ++ ": " ++ v) (columns section))))]
                , if str "path" item /= "" then a [class "btn btn-outline",href (str "path" item)] [text "Open record"] else text ""
                , div [] (List.map (\file -> if str "scanStatus" file=="Clean" then a [class "btn btn-outline",href ("/api/documents/" ++ str "documentId" file ++ "/download")] [text ("Version " ++ str "version" file ++ " · " ++ str "filename" file)] else p [] [text ("Version " ++ str "version" file ++ " · " ++ str "filename" file ++ " · " ++ str "scanStatus" file)]) (items "versions" item))
                , if section=="documents" && canWrite then button [class "btn btn-outline",disabled (not canWrite || model.featureBusy || model.checklistFile==Nothing),onClick (ChecklistUpload (String.toInt(str "id" item) |> Maybe.withDefault 0) (String.toInt(str "revision" item) |> Maybe.withDefault 0))] [text "Upload selected file to this requirement"] else text ""
                ]
            , if List.member section ["history","contact-log","reports","communication","ageing","bottlenecks","trends","sales","cashflow","balances","finance-audit"] || not canWrite then text "" else button [class "btn btn-outline",disabled model.featureBusy,onClick (FeatureEdit item)] [text (if section=="search" then "Select view" else "Select / edit")]
            ]
        extraUsers = D.decodeValue (D.at ["security","users"] (D.list D.value)) model.featureData |> Result.withDefault []
    in
    div []
        [ div [class "page-head"] [div [] [h2 [] [text title],p [class "page-description"] [text description]]]
        , if section=="appointments" then a [class "btn btn-outline",href "/api/workspace/calendar.ics"] [text "Download calendar (.ics)"] else text ""
        , if section=="security" then p [class "notice-banner"] [text "Keep recovery codes somewhere safe. Lost both your authenticator and codes? Another administrator with MFA can reset your account after verifying your identity. Password recovery does not bypass MFA."] else text ""
        , if section=="security" then p [] [text (if D.decodeValue (D.at ["security","enabled"] D.bool) model.featureData == Ok True then "Authenticator protection is enabled." else "Authenticator protection is not enabled. Administrators may be required to set it up before opening records.")] else text ""
        , if not (List.member section ["security","reports","ageing","bottlenecks","trends","sales","cashflow"]) then div [class "form-group"] [label [for "feature-search"] [text "Search this list and record choices"],input [id "feature-search",class "input",value model.featureSearch,onInput FeatureSearch,placeholder "Name, phone, task, case or file…"] []] else text ""
        , if not canWrite then p [class "notice-banner", attribute "role" "status"] [text (if section == "checklist-templates" then "Only administrators can edit templates. Available checklists can be applied from an application." else "You have view-only access. Ask a consultant or administrator to make changes.")] else text ""
        , if List.member section ["reports","ageing","bottlenecks","trends","sales","cashflow"] then
            div [class "notice-banner"] [text ("Active date range: " ++ (if model.featureSearch == "" then (if section == "reports" then "last 30 days" else "all dates") else String.replace "," " to " model.featureSearch) ++ (if section == "reports" then ". Summary exports use this same date range." else ". CSV exports contain up to 10,000 rows; narrow the range for larger reports."))]
          else text ""
        , if get "id" /= "" then p [class "summary-strip"] [text ("Selected: " ++ (if get "title" /= "" then get "title" else if get "name" /= "" then get "name" else "item " ++ get "id"))] else text ""
        , if section == "applications" && model.inlineOwner == "" then applicationOverview model else text ""
        , if section == "search" then
            Html.details [class "saved-view-tools"] [Html.summary [] [text "Saved views (optional)"], div [class "card feature-editor"] (List.map control (fields section |> List.filter (\(_,key,_) -> (model.inlineOwner=="" || key/="document") && (section/="security" || key/="reason" || Maybe.map .role model.user==Just "admin"))) ++ [div [class "form-actions"] (actions ++ [button [class "btn btn-outline",onClick FeatureReset] [text "New / clear form"]])])]
          else
            div [class "card feature-editor"] (List.map control (fields section |> List.filter (\(_,key,_) -> (model.inlineOwner=="" || key/="document") && (section/="security" || key/="reason" || Maybe.map .role model.user==Just "admin"))) ++ [div [class "form-actions"] (actions ++ [button [class "btn btn-outline",onClick FeatureReset] [text "New / clear form"]])])
        , if model.featureError/="" then p [id "feature-error",class "error-banner",attribute "role" "alert"] [text model.featureError] else span [id "feature-error"] []
        , if section=="payments" then a [class "btn btn-outline",href "/work/payment-history"] [text "Search all payment history"] else text ""
        , if section=="reconciliation" && get "id" /= "" then div [class "card"]
            [ h3 [] [text "Matching recorded entries"]
            , p [] [text ("Selected: " ++ get "matchLabel")]
            , label [for "reconcile-search"] [text "Search matching receipts and expense payments"]
            , input [id "reconcile-search",class "input",value model.financeCandidateQuery,onInput FinanceCandidateQuery] []
            , div [] (List.map (\entry -> button [class "btn btn-outline",onClick (FinanceCandidateSelected entry)] [text (str "label" entry ++ " · " ++ str "date" entry)]) model.financeCandidates)
            , button [class "btn btn-outline",disabled (model.financeCandidateOffset==0),onClick (FinanceCandidateStep -1)] [text "Previous matches"]
            , button [class "btn btn-outline",disabled (List.length model.financeCandidates<50),onClick (FinanceCandidateStep 1)] [text "More matches"]
            ] else text ""
        , if section=="expenses" && get "id" /= "" && D.decodeValue (D.at ["permissions","submit"] D.bool) model.featureData == Ok True then div [class "card"]
            [ label [for "financial-file"] [text "Expense receipt or supporting document (PDF, PNG, JPEG; up to 10 MB)"]
            , input [id "financial-file",type_ "file",accept "application/pdf,image/png,image/jpeg",Html.Events.on "change" (D.map FinanceFile (D.at ["target","files"] (D.index 0 File.decoder)))] []
            , button [class "btn btn-outline",disabled (model.featureBusy || model.financeFile==Nothing),onClick (FinanceUpload (String.toInt (get "id") |> Maybe.withDefault 0) (String.toInt (get "expectedRevision") |> Maybe.withDefault 0))] [text "Attach supporting document"]
            ] else text ""
        , if section=="documents" && canWrite then div [class "card"]
            [ label [for "checklist-file"] [text "Upload directly into a checklist item (up to 25 MB)"]
            , input [id "checklist-file",type_ "file",Html.Events.on "change" (D.map ChecklistFile (D.at ["target","files"] (D.index 0 File.decoder)))] []
            , p [] [text (if model.featureBusy then "Uploading or saving… Please wait before leaving this page." else "Choose a file, then upload it to its requirement. Pending scans block download and approval. Replace infected files; ask an administrator to retry failed scans.")]
            , div [class "form-actions"] (List.map (\(filter,labelText) -> button [class "btn btn-outline",onClick(FeatureSearch filter)] [text labelText]) [("review","Review queue"),("expiring","Expiring documents"),("outstanding","Outstanding requirements"),("","All documents")])
            , button [class "btn btn-outline",disabled model.featureBusy,onClick(PageStep 0)] [text "Refresh scan status"]
            ] else text ""
        , if List.member section ["reports","ageing","bottlenecks","trends"] then Reports.view model section else text ""
        , if List.member section ["sales","cashflow","payments","balances","expenses","finance-audit"] && D.decodeValue (D.at ["permissions","export"] D.bool) model.featureData == Ok True then a [class "btn btn-outline",href ("/api/workspace/finance-export/" ++ section ++ "?q=" ++ Url.percentEncode model.featureSearch)] [text "Export CSV (up to 10,000 rows)"] else text ""
        , if List.member section ["payments","balances"] then div [class "summary-strip"] (List.map (\summary -> p [] [text (str "currency" summary ++ " · Outstanding: " ++ str "outstanding" summary ++ " · Overdue: " ++ str "overdue" summary)]) (items "summary" model.featureData) ++ [button [class "btn btn-outline",onClick (FeatureSearch "overdue")] [text "Overdue accounts"],button [class "btn btn-outline",onClick (FeatureSearch "")] [text "All invoices"]]) else text ""
        , if section=="reports" then a [class "btn btn-outline",href ("/api/workspace/reports.csv?q=" ++ Url.percentEncode model.featureSearch)] [text "Export original summary CSV"] else text ""
        , if section=="documents" then div [class "summary-strip"] (List.map (\summary -> p [] [text ("Required: " ++ str "required" summary ++ " · Outstanding: " ++ str "outstanding" summary ++ " · Awaiting review: " ++ str "review" summary ++ " · Expiring: " ++ str "expiring" summary)]) (items "summary" model.featureData)) else text ""
        , if List.member section ["ageing","bottlenecks","trends"] then a [class "btn btn-outline",href ("/api/workspace/report-export/" ++ section ++ "?q=" ++ Url.percentEncode model.featureSearch)] [text "Export original CSV"] else text ""
        , if D.decodeValue (D.null ()) model.featureResult == Ok () then text "" else resultView model.featureResult
        , if model.pageLoading then Ui.spinner
          else if List.isEmpty model.opsData && not (List.member section ["communication","security"]) then
            Ui.emptyState "check" "No matching records" (emptyMessage section model.featureSearch)
          else if List.member section ["sales","cashflow"] then financeReport section model.opsData
          else if List.member section ["reports","ageing","bottlenecks","trends"] then text ""
          else div [class "operations-list"] (List.map row model.opsData)
        , if section=="search" then div [] (List.map row (D.decodeValue (D.list D.value) model.featureResult |> Result.withDefault [])) else text ""
        , div [] (List.map row (items "results" model.featureResult))
        , if section=="security" && Maybe.map .role model.user==Just "admin" then div [class "card"] (h3 [] [text "Team access"] :: p [] [text "Select an account, choose its record access and privileges, then enter your current password above. Administrators retain full access; Viewers remain read-only. The server’s assigned-only setting always takes precedence."] :: List.map (\item -> div [class "operation-row"] [span [] [text (str "name" item ++ " · " ++ str "role" item ++ (if str "suspended" item=="1" then " · Suspended" else " · Active"))],button [class "btn btn-outline",disabled model.featureBusy,onClick (FeatureEdit item)] [text "Select account"]]) extraUsers ++ List.map control (("Record access", "accessLevel", "all|assigned") :: List.map (\(key,labelText) -> (labelText,key,"allowed|denied")) [("finance_periods","Close and reopen financial months"),("finance_reconcile","Reconcile company bank statements"),("sales_view","View sales reports"),("finance_view","View invoices, payments and financial reports"),("payments_record","Record and reverse student payments"),("expenses_record","Submit expenses and record expense payments"),("expenses_approve","Approve, reject and reverse company expenses"),("edit","Create and edit records"),("delete","Delete, archive and bulk update records"),("download","Download files and photos"),("export","Export records and calendars"),("email","Send email to contacts"),("portal","Manage student portal access")]) ++ (if Maybe.map .role model.user==Just "admin" then [action "access" "Save access and privileges",action "suspend" "Suspend selected account",action "resume" "Resume selected account",action "revoke-user" "Revoke selected account sessions",action "reset-mfa" "Reset selected account’s authenticator"] else [])) else text ""
        ]

financeReport : String -> List D.Value -> Html Msg
financeReport section rows_ =
    let
        tableFor title_ keys entries = div [class "card table-card"]
            [ h3 [] [text title_]
            , table [class "data-table"]
                [ thead [] [tr [] (List.map (\key -> th [scope "col"] [text (if key == "title" then "Group" else fieldLabel key)]) keys)]
                , tbody [] (List.map (\entry -> tr [] (List.map (\key -> td [] [text (str key entry)]) keys)) entries)
                ]
            ]
        currencyRows = List.filter (\entry -> str "kind" entry == "Currency") rows_
        moneyColumns = ["currency","invoiced","collected"] ++ (if section == "cashflow" then ["expenses","net"] else []) ++ ["outstanding","overdue","legacyUndated"]
    in
    div []
        ((if section == "sales" then List.map (\kind -> tableFor (if kind == "Source" then "Leads by source" else if kind == "Consultant" then "Leads by consultant" else "Pipeline stages") ["title","leads","converted","conversionRate"] (List.filter (\entry -> str "kind" entry == kind) rows_)) ["Source","Consultant","Pipeline"] else [])
        ++ [tableFor "Amounts by currency" moneyColumns currencyRows])

columns : String -> List String
columns section = case section of
    "payment-history" -> ["currency","amount","paidOn","method","reference","actor","reason","reversesId","reversed"]
    "periods" -> ["status","changedBy","changedAt","reason"]
    "reconciliation" -> ["currency","amount","postedOn","reference","description","status","reason"]
    "sales" -> ["kind","leads","converted","conversionRate","currency","invoiced","collected","outstanding","overdue","legacyUndated"]
    "cashflow" -> ["currency","invoiced","collected","expenses","net","outstanding","overdue","legacyUndated"]
    "payments" -> ["currency","total","paid","balance","dueDate","overdueDays","status"]
    "balances" -> ["currency","total","paid","balance","dueDate","overdueDays","status"]
    "expenses" -> ["currency","amount","category","expenseOn","status","createdBy","reviewNote"]
    "finance-audit" -> ["date","actor","details"]
    "applications" -> ["clientName","institution","program","offerStatus","enrollmentStatus","category","destination","stage","intakeDate","deadline","officer"]
    "milestones" -> ["student","application","kind","status","plannedOn","completedOn","note"]
    "contact-log" -> ["kind","date","actor","note"]
    "checklist-templates" -> ["category","destination","requiredDocuments","optionalDocuments"]
    "tasks" -> ["recordName","assigneeName","status","priority","dueDate"]
    "history" -> ["actor","kind","date","note"]
    "documents" -> ["status","scanStatus","expiryWarning","expiresOn","reviewNote"]
    "mail" -> ["recipient","status","attempts","lastError","nextAttempt","sentAt"]
    "notifications" -> ["kind","status","date"]
    "ageing" -> ["stage","ageDays","overdueDays","staff"]
    "bottlenecks" -> ["cases","averageAgeDays","oldestDays"]
    "trends" -> ["date","openTasks","overdueTasks","openCases"]
    "reports" -> ["from","to","summary"]
    "security" -> ["created","expires"]
    "invoices" -> ["currency","amount","paid","balance","status","dueDate","description"]
    "appointments" -> ["startsAt","endsAt","location","status"]
    _ -> ["status","expires","kind","phone","email"]

resultView : D.Value -> Html Msg
resultView result =
    div [class "card",attribute "role" "status"]
        ([p [] [text (str "message" result)]]
        ++ List.filterMap (\key -> let value_=str key result in if value_=="" then Nothing else Just (div [] [strong [] [text (if key=="secret" then "Authenticator key: " else if key=="uri" then "Authenticator setup URI: " else "Private student sign-in link: ")],code [] [text value_]])) ["secret","uri","url"]
        ++ (if List.isEmpty (items "counts" result) then [] else List.map (\counts -> p [] [text (String.join " · " (List.map (\key -> key ++ ": " ++ str key counts) ["documents","tasks","cases","notes","activities","invoices"]))]) (items "counts" result))
        ++ (D.decodeValue (D.field "recoveryCodes" (D.list D.string)) result |> Result.withDefault [] |> List.map (\value_ -> p [] [code [] [text value_]])))

portalView : Model -> Html Msg
portalView model =
    let
        connected = not (List.isEmpty (items "client" model.portalData))
        block name key render =
            div [class "card"]
                [ h2 [] [text name]
                , if List.isEmpty (items key model.portalData) then p [class "empty-note"] [text (if key == "documents" then "No approved documents have been shared with you yet." else if key == "checklist" then "Your case officer has not requested documents yet." else "Nothing to show yet.")]
                  else div [] (List.map render (items key model.portalData))
                ]
    in
    div [class "portal-container"]
        ([ h1 [] [text "Your student workspace"]
         , if model.portalError /= "" then p [class "error-banner", attribute "role" "alert"] [text model.portalError] else text ""
         , if model.portalNotice /= "" then p [class "notice-banner", attribute "role" "status"] [text model.portalNotice] else text ""
         , if model.portalBusy then p [class "notice-banner", attribute "role" "status"] [text "Please wait… Your request is in progress."] else text ""
         ]
        ++ (if not connected then
                [ p [] [text "Open the private sign-in link supplied by your case officer. Each link can be used once. Access may expire or be revoked; contact your case officer for a replacement."] ]
            else
                [ div [class "form-actions"]
                    [ button [class "btn btn-outline", disabled model.portalBusy, onClick PortalRefresh] [text "Refresh status"]
                    , button [class "btn btn-outline", disabled model.portalBusy, onClick PortalLogout] [text "Sign out"]
                    ]
                , block "Your details" "client" (\item -> p [] [text (str "name" item ++ " · " ++ str "number" item)])
                , block "Applications" "cases" (\item -> p [] [text (str "title" item ++ " · " ++ str "stage" item ++ (if str "deadline" item == "" then "" else " · Next deadline: " ++ str "deadline" item))])
                , div [class "card"]
                    [ label [for "portal-file"] [text "Choose a file to submit (up to 25 MB)"]
                    , input [id "portal-file",type_ "file", disabled model.portalBusy, Html.Events.on "change" (D.map PortalFile (D.at ["target","files"] (D.index 0 File.decoder)))] []
                    , p [] [text (Maybe.map File.name model.portalFile |> Maybe.withDefault "Choose a file, then submit it to the matching requirement below.")]
                    , p [class "status-help"] [text "Files are scanned before review. A rejected document needs a replacement that addresses the feedback. Refresh to see progress."]
                    ]
                , block "Document checklist" "checklist" (\item -> div [class "operation-row"]
                    [ div [] [h3 [] [text (str "title" item)],p [] [text (str "status" item ++ (if str "required" item == "1" then " · Required" else " · Optional"))],p [class "status-help"] [text (scanHelp (str "scanStatus" item))],p [] [text (str "feedback" item)],p [] [text (if str "expiresOn" item == "" then "" else "Expires: " ++ str "expiresOn" item)]]
                    , button [class "btn btn-primary",disabled (model.portalFile==Nothing || model.portalBusy),onClick (PortalUpload (String.toInt (str "id" item) |> Maybe.withDefault 0))] [text (if model.portalBusy then "Please wait…" else "Submit selected file")]
                    ])
                , block "Shared documents" "documents" (\item -> a [class "btn btn-outline",href ("/api/portal/documents/" ++ str "id" item)] [text (str "title" item)])
                , block "Invoices" "invoices" (\item -> p [] [text ("INV-" ++ str "id" item ++ " · " ++ str "description" item ++ " · " ++ str "currency" item ++ " " ++ str "amount" item ++ " · " ++ str "status" item ++ " · Paid " ++ str "paid" item ++ " · Balance " ++ str "balance" item ++ " · Due " ++ str "dueDate" item)])
                ]
           )
        )


reportRow : D.Value -> Html Msg
reportRow item =
    let
        n key = D.decodeValue (D.field key D.float) item |> Result.withDefault 0
        percentage = if n "leads" == 0 then "0" else String.fromInt (round (100 * n "converted" / n "leads"))
    in
    div []
        (List.map (\(key,labelText) -> p [] [strong [] [text (labelText ++ ": ")],text (str key item)]) [("leads","Leads in cohort"),("converted","Converted leads"),("openTasks","Open tasks"),("overdueTasks","Overdue tasks"),("completedCases","Applications completed"),("averageCaseDays","Average application days")]
        ++ [p [] [text ("Conversion rate: " ++ percentage ++ "%")]])

embedded : Model -> Int -> Html Msg
embedded model cid =
    div [class "inline-workspace",attribute "aria-label" "Student and application work"]
        [ h2 [] [text "Work on this record or application"]
        , label [for "case-work-scope"] [text "Scope"]
        , select [id "case-work-scope",class "input",disabled (model.inlineSection == "applications"),value model.inlineOwner,onInput InlineOwner]
            (option [value ("client:" ++ String.fromInt cid),selected (model.inlineOwner=="client:" ++ String.fromInt cid)] [text "Whole student"] ::
             (Maybe.map .cases model.clientDetail |> Maybe.withDefault [] |> List.map (\c -> option [value ("case:" ++ String.fromInt c.id),selected(model.inlineOwner=="case:" ++ String.fromInt c.id)] [text (Maybe.withDefault "Application" c.category ++ " · " ++ c.stage ++ " · " ++ String.fromInt c.id)])))
        , div [class "form-actions",attribute "aria-label" "Student work sections"] (List.map (\(section,labelText) -> button [class (if model.inlineSection==section then "btn btn-primary" else "btn btn-outline"),attribute "aria-pressed" (if model.inlineSection==section then "true" else "false"),onClick(InlineSection section)] [text labelText]) [("applications","Applications"),("tasks","Edit tasks"),("history","Conversation history"),("documents","Document checklist")])
        , Html.details []
            [ Html.summary [] [text "Scheduling and billing"]
            , div [class "form-actions"]
                [ a [class "btn btn-outline",href "/work/appointments"] [text "Appointments"]
                , if Maybe.map .role model.user == Just "admin" then a [class "btn btn-outline",href "/work/invoices"] [text "Invoices"] else text ""
                ]
            ]
        , view model model.inlineSection
        , div [class "form-actions"]
            [button [class "btn btn-outline",disabled(model.pageOffset==0),onClick(PageStep -1)] [text "Previous work page"],button [class "btn btn-outline",disabled(List.length model.opsData<50),onClick(PageStep 1)] [text "Next work page"]]
        ]

conflictView : Model -> Html Msg
conflictView model =
    let
        latest = formValues model.conflictCurrent
        hidden key = List.member key ["id","revision","expectedRevision","request","session","user","password","uuPassword"]
        get values_ key = Dict.get key values_ |> Maybe.withDefault ""
        changed = Dict.keys latest |> List.filter (\key -> not(hidden key) && get model.conflictOriginal key /= get latest key)
        cell_ values_ key = td [] [text (get values_ key)]
        workflow = List.member model.conflictKind ["tasks","documents","reminders"]
    in
    if D.decodeValue (D.field "revision" D.int) model.conflictCurrent |> Result.toMaybe |> (==) Nothing then text "" else
    Ui.modal "Review changes before saving" (div []
        [ p [] [text "Your save was rejected because this record changed. Review the original, your draft and the latest version. Accepting a version does not submit the form."]
        , div [class "table-scroll"] [table [class "data-table"]
            [ caption [] [text "Fields changed since the form was opened"]
            , thead [] [tr [] (List.map (\labelText -> th [scope "col"] [text labelText]) ["Field","Original","Your draft","Latest","Action"])]
            , tbody [] (List.map (\key -> tr [] [th [scope "row"] [text (fieldLabel key)],cell_ model.conflictOriginal key,cell_ model.conflictEdits key,cell_ latest key,td [] [if workflow then button [class "btn btn-outline",onClick(ConflictUseCurrent key)] [text "Use latest value"] else text "Review in your form"]]) changed)
            ]]
        , p [] [text (if List.isEmpty changed then "The revision changed because related work changed. Your fields are unchanged in the latest version." else "Keeping your draft can replace the latest values for these fields.")]
        , button [class "btn btn-primary",onClick (ConfirmRequested "Use the latest version number with the reviewed draft? Save the form separately when ready." ConflictRebase)] [text "Accept comparison and return to form"]
        , button [class "btn btn-outline",onClick ConflictDismiss] [text "Keep reviewing later"]
        ])

fieldLabel : String -> String
fieldLabel key =
    case key of
        "conversionRate" -> "Conversion (%)"
        "legacyUndated" -> "Legacy payments with unknown dates"
        "invoiced" -> "Invoiced in period"
        "collected" -> "Net receipts in period"
        "outstanding" -> "Current outstanding"
        "overdue" -> "Current overdue"
        "expenses" -> "Paid expenses in period"
        "net" -> "Receipts less paid expenses"
        "createdBy" -> "Submitted by"
        "expenseOn" -> "Expense date"
        "reviewNote" -> "Review note"
        "total" -> "Invoice total"
        "paid" -> "Net payments"
        "balance" -> "Outstanding balance"
        "clientName" -> "Student"
        "institution" -> "School"
        "program" -> "Program"
        "offerStatus" -> "Offer"
        "enrollmentStatus" -> "Enrollment"
        "plannedOn" -> "Planned date"
        "completedOn" -> "Completed date"
        "category" -> "Application type"
        "intakeDate" -> "Intake date"
        "deadline" -> "Next deadline"
        "officer" -> "Officer"
        "requiredDocuments" -> "Required documents"
        "optionalDocuments" -> "Optional documents"
        "ncFullName" -> "Student name"
        "ncEmail" -> "Email"
        "ncPhone" -> "Phone"
        "ncCaseOfficer" -> "Case officer"
        "ciServiceCategory" -> "Service"
        "ciDestination" -> "Destination"
        "ciStage" -> "Application stage"
        "ciNextDeadline" -> "Deadline"
        "ulFullName" -> "Lead name"
        "ulConsultantId" -> "Consultant"
        "uuName" -> "Staff name"
        "uuRole" -> "Role"
        "ageDays" -> "Age (days)"
        "overdueDays" -> "Overdue (days)"
        "averageAgeDays" -> "Average days in stage"
        "oldestDays" -> "Longest time in stage (days)"
        "openTasks" -> "Open tasks"
        "overdueTasks" -> "Overdue tasks"
        "openCases" -> "Open applications"
        "scanStatus" -> "Malware scan"
        "expiryWarning" -> "Expiry warning"
        "expiresOn" -> "Expires"
        "lastError" -> "Last error"
        "nextAttempt" -> "Next attempt"
        "sentAt" -> "Accepted at"
        _ -> key |> String.toList |> List.concatMap (\c -> if Char.isUpper c then [' ',Char.toLower c] else [c]) |> String.fromList |> String.trim


emptyMessage : String -> String -> String
emptyMessage section query =
    if section == "trends" then "No historical snapshots exist for this range. Enable the scheduler; history starts when snapshots are collected and cannot be reconstructed for earlier dates."
    else if List.member section ["reports","ageing","bottlenecks"] then "There is no data for this range. Try a broader date range; an empty report does not mean the request failed."
    else if query /= "" then "Try another search, clear the filter, or check your record access."
    else case section of
        "checklist-templates" -> "No checklist templates match this view. An administrator can create a reusable checklist here."
        "applications" -> "No applications match this view. Choose a student and complete the form to create their first application, or clear the search to see more records."
        "documents" -> "Choose a record and add its document requirements. Uploaded files must pass scanning before review or download."
        "mail" -> "No messages are visible yet. Invitations, reminders and contact emails appear here after they are queued."
        "portal" -> "Choose a student to create a private, one-use sign-in link. Only approved files you explicitly share become available."
        "notifications" -> "You have no notifications. Preferences determine which categories appear."
        "tasks" -> "Create a task from Tasks or from a lead or student record."
        _ -> "New records will appear here. Your permissions determine what you can see."


scanHelp : String -> String
scanHelp status =
    case status of
        "Pending" -> "Waiting for malware scanning. Downloads and approval remain blocked. Refresh to check progress."
        "Scanning" -> "Malware scanning is in progress. Downloads and approval remain blocked."
        "Error" -> "Scanning failed. An administrator can select this requirement and retry the scan."
        "Infected" -> "This file was blocked by malware scanning. Upload a safe replacement."
        "Clean" -> "Malware scan passed. Your case officer can review the document."
        _ -> "Add a file to this requirement to begin scanning and review."


mailHelp : String -> String
mailHelp status =
    case status of
        "Queued" -> "Waiting for the mail worker. If messages stay queued, ask an administrator to check the worker and SMTP configuration."
        "Retry scheduled" -> "A delivery attempt failed. The worker will retry at the scheduled time."
        "Sending" -> "The mail worker is sending this message."
        "Failed" -> "Automatic attempts are exhausted. An administrator should correct the delivery problem, select this message, and request a retry."
        "Accepted by SMTP" -> "Your mail server accepted this message. This does not confirm arrival in the recipient’s inbox."
        _ -> "Check the status and next-attempt time for delivery progress."


clientPhotoCard : Model -> Int -> Html Msg
clientPhotoCard model cid =
    let
        photo = D.decodeValue (D.field "photo" D.value) model.clientProfile |> Result.withDefault model.clientProfile
        scan = str "scanStatus" photo
        writable = Maybe.map .role model.user /= Just "viewer"
    in
    div [class "card client-photo-card"]
        [ h3 [] [text "Student photo"]
        , div [class "client-photo-row"]
            [ if scan == "Clean" then button [type_ "button",class "photo-preview-trigger",attribute "aria-label" "Enlarge student profile photo"] [img [src ("/api/clients/" ++ String.fromInt cid ++ "/photo?v=" ++ str "documentId" photo),alt "Student profile photo",class "client-photo"] []]
              else div [class "client-photo-placeholder",attribute "aria-label" "No approved profile photo"] [text "Photo"]
            , p [class "status-help"] [text (if str "documentId" photo == "" then "Add a PNG or JPEG profile photo up to 2 MB and 4096 × 4096 pixels." else scanHelp scan)]
            ]
        , if model.clientPhotoError /= "" then p [class "error-banner",attribute "role" "alert"] [text model.clientPhotoError] else text ""
        , if model.clientPhotoNotice /= "" then p [class "notice-banner",attribute "role" "status"] [text model.clientPhotoNotice] else text ""
        , if writable then div [class "form-group"]
            [ label [for "client-photo-file"] [text "Student profile photo"]
            , input [id "client-photo-file",type_ "file",accept "image/png,image/jpeg",disabled model.clientPhotoBusy,Html.Events.on "change" (D.map ClientPhotoPicked (D.at ["target","files"] (D.index 0 File.decoder)))] []
            , case model.clientPhotoPreview of
                Just url -> button [type_ "button",class "selected-photo-preview photo-preview-trigger",attribute "aria-label" "Preview selected picture"] [img [src url,alt "Selected picture preview"] []]
                Nothing -> text ""
            , p [] [text (Maybe.map File.name model.clientPhotoFile |> Maybe.withDefault "No replacement photo selected")]
            , button [class "btn btn-primary",disabled (model.clientPhotoBusy || model.clientPhotoFile==Nothing),onClick (ClientPhotoUpload cid)] [text (if model.clientPhotoBusy then "Uploading…" else "Upload photo")]
            ] else text ""
        , div [class "form-actions"]
            [ button [class "btn btn-outline",disabled model.clientPhotoBusy,onClick(ClientProfileRefresh cid)] [text "Refresh photo status"]
            , if scan == "Error" && Maybe.map .role model.user==Just "admin" then button [class "btn btn-outline",disabled model.clientPhotoBusy,onClick(ClientPhotoRetry cid)] [text "Retry failed photo scan"] else text ""
            ]
        ]


applicationOverview : Model -> Html Msg
applicationOverview model =
    let
        filterValue key = Dict.get key model.applicationFilters |> Maybe.withDefault ""
        optionFilter key labelText kind =
            div [class "form-group"]
                [ label [for ("application-filter-" ++ key)] [text labelText]
                , select [id ("application-filter-" ++ key),value (filterValue key),disabled model.featureBusy,onInput (ApplicationFilter key)]
                    (option [value ""] [text ("All " ++ String.toLower labelText)] :: List.map (\(v,l) -> option [value v,selected (filterValue key == v)] [text l]) kind)
                ]
        options prefix = items "options" model.featureData |> List.filter (\item -> String.startsWith (prefix ++ ":") (str "value" item)) |> List.map (\item -> (String.dropLeft (String.length prefix + 1) (str "value" item),str "label" item))
        summary = items "summary" model.featureData |> List.head
        metric key labelText = div [class "stat"] [div [class "stat-label"] [text labelText],div [class "stat-value"] [text (summary |> Maybe.map (str key) |> Maybe.withDefault "0")]]
        deadlines key title =
            div [class "card"]
                [ h3 [] [text title]
                , if List.isEmpty (items key model.featureData) then p [class "empty-note"] [text (if key == "overdue" then "No overdue applications in this view." else "No application deadlines in the next seven days.")]
                  else div [] (List.map (\item -> div [class "operation-row"]
                    [ div [] [h3 [] [text (str "title" item)],p [] [text (str "stage" item ++ " · " ++ str "deadline" item)]]
                    , button [class "btn btn-outline",disabled model.featureBusy,onClick(FeatureEdit item)] [text "Review application"]
                    ]) (items key model.featureData))
                ]
    in
    div [class "application-overview",attribute "aria-busy" (if model.pageLoading then "true" else "false")]
        [ div [class "application-filters card"]
            [ optionFilter "client" "Students" (options "client")
            , optionFilter "officer" "Officers" (options "staff")
            , optionFilter "destination" "Destinations" (List.map (\v -> (v,v)) ["Canada","Australia","New Zealand","UAE","UK","Schengen"])
            , optionFilter "stage" "Stages" (List.map (\v -> (v,v)) ["Documents Under Review","Submitted","Decision Pending","Approved","Rejected"])
            , div [class "form-group"] [label [for "application-filter-intake"] [text "Intake month"],input [id "application-filter-intake",type_ "month",value (filterValue "intake"),disabled model.featureBusy,onInput (ApplicationFilter "intake")] []]
            , button [class "btn btn-outline",disabled model.featureBusy,onClick ClearApplicationFilters] [text "Clear filters"]
            ]
        , if model.pageLoading then p [class "status-help",attribute "role" "status"] [text "Updating application overview…"] else text ""
        , div [class "stats"] [metric "total" "Applications",metric "upcoming" "Due in the next 7 days",metric "overdue" "Overdue applications"]
        , div [class "stage-counts",attribute "aria-label" "Application counts by stage"]
            (List.map (\item -> button [class "btn btn-outline",disabled model.featureBusy,onClick(ApplicationFilter "stage" (str "stage" item))] [text (str "stage" item ++ " · " ++ str "count" item)]) (items "stages" model.featureData))
        , div [class "application-deadlines"] [deadlines "upcoming" "Upcoming deadlines",deadlines "overdue" "Overdue work"]
        , p [class "status-help"] [text "Counts and deadlines follow the current filters and your record permissions. Each deadline panel shows up to five applications. Approved and rejected applications are excluded from overdue work."]
        ]


clientNextSteps : Model -> Int -> Html Msg
clientNextSteps model cid =
    let
        steps = D.decodeValue (D.field "nextSteps" D.value) model.clientProfile |> Result.withDefault model.clientProfile
        application = D.decodeValue (D.field "application" D.value) steps |> Result.withDefault steps
        task = D.decodeValue (D.field "task" D.value) steps |> Result.withDefault steps
        waiting = D.decodeValue (D.null ()) model.clientProfile == Ok ()
    in
    div [class "card client-next-steps"]
        [ h3 [] [text "Student at a glance"]
        , if waiting then div [] [p [class "status-help"] [text (if model.clientPhotoError == "" then "Loading the latest application and task…" else "Next steps could not be loaded. Refresh to try again.")], if model.clientPhotoError /= "" then button [class "btn btn-outline",onClick(ClientProfileRefresh cid)] [text "Retry next steps"] else text ""]
          else div [class "next-step-grid"]
            [ div [] [h3 [] [text "Current application"],p [] [text (if str "id" application == "" then "No active application. Create one when the student is ready." else String.join " · " (List.filter ((/=) "") [str "category" application,str "destination" application,str "stage" application]))],p [] [text (String.join " · " (List.filter ((/=) "") [str "institution" application,str "program" application,str "offerStatus" application,str "enrollmentStatus" application]))],p [] [text (if str "deadline" application == "" then "No application deadline set." else "Next deadline: " ++ str "deadline" application)],a [class "btn btn-outline",href ("/applications?client=" ++ String.fromInt cid)] [text "Manage applications"]]
            , div [] [h3 [] [text "Next action"],p [] [text (if str "id" task == "" then "No open task. Add a next step below." else str "title" task)],p [] [text (if str "dueDate" task == "" then "No task deadline set." else "Due: " ++ str "dueDate" task)],a [class "btn btn-outline",href "#client-tasks"] [text "View student tasks"]]
            , div [] [h3 [] [text "Missing requirements"],p [] [text ((if str "outstandingDocuments" steps == "" then "0" else str "outstandingDocuments" steps) ++ " required documents still need approval.")],ul [class "requirement-summary"] (List.map (\item -> li [] [text (str "title" item ++ " · " ++ str "status" item)]) (items "requirements" steps)),button [class "btn btn-outline",onClick(InlineSection "documents")] [text "Review checklist"]]
            , div [] [h3 [] [text "Upcoming milestones"], if List.isEmpty (items "milestones" steps) then p [] [text "No open milestones."] else ul [] (List.map (\item -> li [] [text (str "title" item ++ " · " ++ (if str "plannedOn" item == "" then "No date set" else str "plannedOn" item))]) (items "milestones" steps)),a [class "btn btn-outline",href "/work/milestones"] [text "Manage milestones"]]
            , div [] [h3 [] [text "Assigned officer"],p [] [text (model.clientDetail |> Maybe.andThen (.client >> .caseOfficerName) |> Maybe.withDefault "Unassigned")],if Maybe.map .role model.user /= Just "viewer" then button [class "btn btn-outline",onClick(OpenClientEditor cid)] [text "Edit assignment"] else text ""]
            ]
        ]
