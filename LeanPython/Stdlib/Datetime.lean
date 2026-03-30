import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Datetime

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- timedelta method dispatch
-- ============================================================

/-- Dispatch methods on datetime.timedelta instances. -/
partial def callTimedeltaMethod (iref : HeapRef) (method : String)
    (args : List Value) : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let days := match id_.attrs["_days"]? with
    | some (.int n) => n
    | some (.float f) => f.toUInt64.toNat
    | _ => 0
  let seconds := match id_.attrs["_seconds"]? with
    | some (.int n) => n
    | some (.float f) => f.toUInt64.toNat
    | _ => 0
  let microseconds := match id_.attrs["_microseconds"]? with
    | some (.int n) => n
    | _ => 0
  match method with
  | "total_seconds" =>
    match args with
    | [] =>
      let total : Float := Float.ofInt days * 86400.0 +
        Float.ofInt seconds + Float.ofInt microseconds / 1000000.0
      return .float total
    | _ => throwTypeError "total_seconds() takes no arguments"
  | "__str__" | "__repr__" =>
    let totalSec := days * 86400 + seconds
    let h := totalSec / 3600
    let m := (totalSec % 3600) / 60
    let s := totalSec % 60
    if days == 0 then
      return .str s!"{h}:{String.ofList (padLeft2 m)}{m}:{String.ofList (padLeft2 s)}{s}"
    else
      return .str s!"datetime.timedelta(days={days}, seconds={seconds})"
  | _ => throwAttributeError s!"'timedelta' object has no attribute '{method}'"
where
  padLeft2 (n : Int) : List Char :=
    if n < 10 && n >= 0 then ['0'] else []

-- ============================================================
-- datetime method dispatch
-- ============================================================

/-- Dispatch methods on datetime.datetime instances. -/
partial def callDatetimeMethod (iref : HeapRef) (method : String)
    (_args : List Value) : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let year := match id_.attrs["_year"]? with | some (.int n) => n | _ => 1970
  let month := match id_.attrs["_month"]? with | some (.int n) => n | _ => 1
  let day := match id_.attrs["_day"]? with | some (.int n) => n | _ => 1
  let hour := match id_.attrs["_hour"]? with | some (.int n) => n | _ => 0
  let minute := match id_.attrs["_minute"]? with | some (.int n) => n | _ => 0
  let second := match id_.attrs["_second"]? with | some (.int n) => n | _ => 0
  match method with
  | "isoformat" =>
    let sep := "T"
    return .str s!"{pad4 year}-{pad2 month}-{pad2 day}{sep}{pad2 hour}:{pad2 minute}:{pad2 second}"
  | "__str__" =>
    return .str s!"{pad4 year}-{pad2 month}-{pad2 day} {pad2 hour}:{pad2 minute}:{pad2 second}"
  | "__repr__" =>
    return .str s!"datetime.datetime({year}, {month}, {day}, {hour}, {minute}, {second})"
  | "year" => return .int year
  | "month" => return .int month
  | "day" => return .int day
  | "hour" => return .int hour
  | "minute" => return .int minute
  | "second" => return .int second
  | "timestamp" =>
    -- Simplified: return seconds since epoch (very rough)
    let daysSinceEpoch := (year - 1970) * 365 + (month - 1) * 30 + (day - 1)
    let totalSec := daysSinceEpoch * 86400 + hour * 3600 + minute * 60 + second
    return .float (Float.ofInt totalSec)
  | "replace" => return .instance iref  -- simplified: just return self
  | _ => throwAttributeError s!"'datetime' object has no attribute '{method}'"
where
  pad2 (n : Int) : String :=
    if n >= 0 && n < 10 then s!"0{n}" else toString n
  pad4 (n : Int) : String :=
    if n >= 0 && n < 10 then s!"000{n}"
    else if n >= 10 && n < 100 then s!"00{n}"
    else if n >= 100 && n < 1000 then s!"0{n}"
    else toString n

-- ============================================================
-- timezone method dispatch
-- ============================================================

/-- Dispatch methods on datetime.timezone instances. -/
partial def callTimezoneMethod (_iref : HeapRef) (method : String)
    (_args : List Value) : InterpM Value := do
  match method with
  | "__str__" | "__repr__" => return .str "UTC"
  | "tzname" => return .str "UTC"
  | "utcoffset" => return .none
  | _ => throwAttributeError s!"'timezone' object has no attribute '{method}'"

end LeanPython.Stdlib.Datetime
