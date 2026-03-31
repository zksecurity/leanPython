import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.IO

open LeanPython.Runtime
open LeanPython.Interpreter

-- ============================================================
-- BytesIO method dispatch
-- ============================================================

/-- Dispatch a method call on a BytesIO instance.
    The instance stores `_buffer` (bytes) and `_pos` (int) in its attrs. -/
partial def callBytesIOMethod (iref : HeapRef) (method : String) (args : List Value)
    : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let buf := match id_.attrs["_buffer"]? with
    | some (.bytes b) => b
    | _ => ByteArray.empty
  let pos := match id_.attrs["_pos"]? with
    | some (.int n) => n.toNat
    | _ => 0
  match method with
  | "write" => do
    -- Extract bytes from the argument (raw .bytes or instance with wrappedValue)
    let data ← match args with
    | [.bytes data] => pure data
    | [.instance wref] => do
      let wid ← heapGetInstanceData wref
      match wid.wrappedValue with
      | some (.bytes data) => pure data
      | _ => throwTypeError "BytesIO.write() requires a bytes argument"
    | _ => throwTypeError "BytesIO.write() requires a bytes argument"
    -- Write data at current position, extending buffer if needed
    let mut newBuf := buf
    -- Extend buffer if position is past end
    while newBuf.size < pos do
      newBuf := newBuf.push 0
    -- Write the data starting at pos
    let mut writeBuf := ByteArray.empty
    -- Copy bytes before pos
    for i in [:min pos newBuf.size] do
      writeBuf := writeBuf.push (newBuf.get! i)
    -- Pad if pos > original size
    while writeBuf.size < pos do
      writeBuf := writeBuf.push 0
    -- Write the new data
    for i in [:data.size] do
      writeBuf := writeBuf.push (data.get! i)
    -- Copy remaining bytes after write region
    let afterPos := pos + data.size
    if afterPos < newBuf.size then
      for i in [afterPos:newBuf.size] do
        writeBuf := writeBuf.push (newBuf.get! i)
    let newPos := pos + data.size
    let attrs := id_.attrs
      |>.insert "_buffer" (.bytes writeBuf)
      |>.insert "_pos" (.int newPos)
    heapSetInstanceData iref { id_ with attrs := attrs }
    return .int data.size
  | "read" => do
    let readSize := match args with
      | [.int n] => if n < 0 then buf.size - pos else min n.toNat (buf.size - pos)
      | [] => buf.size - pos  -- read all remaining
      | _ => buf.size - pos
    let startPos := min pos buf.size
    let endPos := min (startPos + readSize) buf.size
    let mut result := ByteArray.empty
    for i in [startPos:endPos] do
      result := result.push (buf.get! i)
    let attrs := id_.attrs.insert "_pos" (.int endPos)
    heapSetInstanceData iref { id_ with attrs := attrs }
    return .bytes result
  | "getvalue" => do
    return .bytes buf
  | "seek" => do
    match args with
    | [.int offset] =>
      -- whence defaults to 0 (SEEK_SET)
      let newPos := max 0 offset
      let attrs := id_.attrs.insert "_pos" (.int newPos)
      heapSetInstanceData iref { id_ with attrs := attrs }
      return .int newPos
    | [.int offset, .int whence] =>
      let newPos := match whence.toNat with
        | 0 => max 0 offset  -- SEEK_SET
        | 1 => max 0 (Int.ofNat pos + offset)  -- SEEK_CUR
        | 2 => max 0 (Int.ofNat buf.size + offset)  -- SEEK_END
        | _ => Int.ofNat pos
      let attrs := id_.attrs.insert "_pos" (.int newPos)
      heapSetInstanceData iref { id_ with attrs := attrs }
      return .int newPos
    | _ => throwTypeError "BytesIO.seek() requires an integer argument"
  | "tell" => do
    return .int pos
  | "__enter__" => return .instance iref
  | "__exit__" => return .none
  | "close" => return .none
  | _ => throwAttributeError s!"'BytesIO' object has no attribute '{method}'"

-- ============================================================
-- StringIO method dispatch
-- ============================================================

/-- Dispatch a method call on a StringIO instance. -/
partial def callStringIOMethod (iref : HeapRef) (method : String) (args : List Value)
    : InterpM Value := do
  let id_ ← heapGetInstanceData iref
  let buf := match id_.attrs["_buffer"]? with
    | some (.str s) => s
    | _ => ""
  let pos := match id_.attrs["_pos"]? with
    | some (.int n) => n.toNat
    | _ => 0
  match method with
  | "write" => do
    match args with
    | [.str data] =>
      let bufChars := buf.toList
      let before := String.ofList (bufChars.take pos)
      let after := String.ofList (bufChars.drop (pos + data.length))
      let padding := String.ofList (List.replicate (pos - min pos bufChars.length) ' ')
      let newBuf := before ++ padding ++ data ++ after
      let newPos := pos + data.length
      let attrs := id_.attrs
        |>.insert "_buffer" (.str newBuf)
        |>.insert "_pos" (.int newPos)
      heapSetInstanceData iref { id_ with attrs := attrs }
      return .int data.length
    | _ => throwTypeError "StringIO.write() requires a string argument"
  | "read" => do
    let bufChars := buf.toList
    let remaining := bufChars.drop pos
    let readChars := match args with
      | [.int n] => if n < 0 then remaining else remaining.take n.toNat
      | [] => remaining
      | _ => remaining
    let newPos := pos + readChars.length
    let attrs := id_.attrs.insert "_pos" (.int newPos)
    heapSetInstanceData iref { id_ with attrs := attrs }
    return .str (String.ofList readChars)
  | "getvalue" => return .str buf
  | "seek" => do
    match args with
    | [.int offset] =>
      let newPos := max 0 offset
      let attrs := id_.attrs.insert "_pos" (.int newPos)
      heapSetInstanceData iref { id_ with attrs := attrs }
      return .int newPos
    | [.int offset, .int whence] =>
      let newPos := match whence.toNat with
        | 0 => max 0 offset
        | 1 => max 0 (Int.ofNat pos + offset)
        | 2 => max 0 (Int.ofNat buf.length + offset)
        | _ => Int.ofNat pos
      let attrs := id_.attrs.insert "_pos" (.int newPos)
      heapSetInstanceData iref { id_ with attrs := attrs }
      return .int newPos
    | _ => throwTypeError "StringIO.seek() requires an integer argument"
  | "tell" => return .int pos
  | "__enter__" => return .instance iref
  | "__exit__" => return .none
  | "close" => return .none
  | _ => throwAttributeError s!"'StringIO' object has no attribute '{method}'"

end LeanPython.Stdlib.IO
