import LeanPython.Interpreter.Types
import Std.Data.HashMap

set_option autoImplicit false

namespace LeanPython.Stdlib.Pydantic

open LeanPython.Runtime

-- ============================================================
-- PydanticConfig: parsed representation of model_config = ConfigDict(...)
-- ============================================================

structure PydanticConfig where
  frozen : Bool := false
  extra : String := "ignore"    -- "ignore" | "allow" | "forbid"
  strict : Bool := false
  populateByName : Bool := false
  validateDefault : Bool := false
  arbitraryTypesAllowed : Bool := false

instance : Inhabited PydanticConfig where
  default := {}

/-- Parse a ConfigDict value (stored as a dict on the heap) into PydanticConfig.
    The dict contains string keys with bool/str values from ConfigDict(...) kwargs. -/
def parsePydanticConfigFromPairs (pairs : Array (Value × Value)) : PydanticConfig :=
  pairs.foldl (fun cfg (k, v) =>
    match k, v with
    | .str "frozen", .bool b => { cfg with frozen := b }
    | .str "extra", .str s => { cfg with extra := s }
    | .str "strict", .bool b => { cfg with strict := b }
    | .str "populate_by_name", .bool b => { cfg with populateByName := b }
    | .str "validate_default", .bool b => { cfg with validateDefault := b }
    | .str "arbitrary_types_allowed", .bool b => { cfg with arbitraryTypesAllowed := b }
    | _, _ => cfg
  ) {}

end LeanPython.Stdlib.Pydantic
