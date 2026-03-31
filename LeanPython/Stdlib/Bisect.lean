import LeanPython.Interpreter.Types
import LeanPython.Runtime.Ops

set_option autoImplicit false

namespace LeanPython.Stdlib.Bisect

open LeanPython.Runtime
open LeanPython.Runtime.Ops
open LeanPython.Interpreter
open LeanPython.AST (CmpOp)

-- ============================================================
-- Helper: bisect_left core logic
-- ============================================================

private partial def bisectLeftCore (arr : Array Value) (x : Value) (lo hi : Nat)
    : InterpM Nat := do
  let mut low := lo
  let mut high := hi
  while low < high do
    let mid := (low + high) / 2
    if mid >= arr.size then break
    let cmp ← evalCmpOp .lt arr[mid]! x
    if cmp then
      low := mid + 1
    else
      high := mid
  return low

-- ============================================================
-- Helper: bisect_right core logic
-- ============================================================

private partial def bisectRightCore (arr : Array Value) (x : Value) (lo hi : Nat)
    : InterpM Nat := do
  let mut low := lo
  let mut high := hi
  while low < high do
    let mid := (low + high) / 2
    if mid >= arr.size then break
    let cmp ← evalCmpOp .lt x arr[mid]!
    if cmp then
      high := mid
    else
      low := mid + 1
  return low

-- ============================================================
-- bisect_left
-- ============================================================

/-- Python bisect.bisect_left(a, x, lo=0, hi=len(a)) -/
partial def bisectLeft (args : List Value) (kwargs : List (String × Value))
    : InterpM Value := do
  match args with
  | .list ref :: x :: rest => do
    let arr ← heapGetList ref
    let lo := match rest with
      | .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "lo") with
        | some (_, .int n) => n.toNat
        | _ => 0
    let hi := match rest with
      | _ :: .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "hi") with
        | some (_, .int n) => n.toNat
        | _ => arr.size
    let result ← bisectLeftCore arr x lo hi
    return .int result
  | .tuple arr :: x :: rest => do
    let lo := match rest with
      | .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "lo") with
        | some (_, .int n) => n.toNat
        | _ => 0
    let hi := match rest with
      | _ :: .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "hi") with
        | some (_, .int n) => n.toNat
        | _ => arr.size
    let result ← bisectLeftCore arr x lo hi
    return .int result
  | _ => throwTypeError "bisect_left() requires a list or tuple and a value"

-- ============================================================
-- bisect_right
-- ============================================================

/-- Python bisect.bisect_right(a, x, lo=0, hi=len(a)) -/
partial def bisectRight (args : List Value) (kwargs : List (String × Value))
    : InterpM Value := do
  match args with
  | .list ref :: x :: rest => do
    let arr ← heapGetList ref
    let lo := match rest with
      | .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "lo") with
        | some (_, .int n) => n.toNat
        | _ => 0
    let hi := match rest with
      | _ :: .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "hi") with
        | some (_, .int n) => n.toNat
        | _ => arr.size
    let result ← bisectRightCore arr x lo hi
    return .int result
  | .tuple arr :: x :: rest => do
    let lo := match rest with
      | .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "lo") with
        | some (_, .int n) => n.toNat
        | _ => 0
    let hi := match rest with
      | _ :: .int n :: _ => n.toNat
      | _ => match kwargs.find? (fun p => p.1 == "hi") with
        | some (_, .int n) => n.toNat
        | _ => arr.size
    let result ← bisectRightCore arr x lo hi
    return .int result
  | _ => throwTypeError "bisect_right() requires a list or tuple and a value"

-- ============================================================
-- insort
-- ============================================================

/-- Python bisect.insort(a, x) — insert x into sorted list a in-place. -/
partial def bisectInsort (args : List Value) (kwargs : List (String × Value))
    : InterpM Value := do
  match args with
  | [.list ref, x] => do
    let idxVal ← bisectRight [.list ref, x] kwargs
    match idxVal with
    | .int idx => do
      let arr ← heapGetList ref
      let i := idx.toNat
      let newArr := (arr.toList.take i ++ [x] ++ arr.toList.drop i).toArray
      heapSetList ref newArr
      return .none
    | _ => throwTypeError "bisect.insort: unexpected error"
  | _ => throwTypeError "bisect.insort() requires a list and a value"

end LeanPython.Stdlib.Bisect
