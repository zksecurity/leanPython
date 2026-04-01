import LeanPython.Interpreter.Types

set_option autoImplicit false

namespace LeanPython.Stdlib.Asyncio

open LeanPython.Runtime
open LeanPython.Interpreter

-- asyncio.sleep(seconds) → coroutine wrapping None
partial def asyncioSleep (_args : List Value) : InterpM Value :=
  return .coroutine .none

-- asyncio.run(coro) → unwrap coroutine
partial def asyncioRun (args : List Value) : InterpM Value :=
  match args with
  | [.coroutine v] => return v
  | [v] => return v
  | _ => throwTypeError "asyncio.run() takes exactly 1 argument"

-- asyncio.gather(*coros) → list of unwrapped values
partial def asyncioGather (args : List Value) : InterpM Value := do
  let results := args.map fun
    | .coroutine v => v
    | v => v
  allocList results.toArray

-- asyncio.create_task(coro) → unwrapped value (eager execution)
partial def asyncioCreateTask (args : List Value) : InterpM Value :=
  match args with
  | [.coroutine v] => return v
  | [v] => return v
  | _ => throwTypeError "asyncio.create_task() takes exactly 1 argument"

-- asyncio.wait_for(coro, timeout) → unwrapped value
partial def asyncioWaitFor (args : List Value)
    (_kwargs : List (String × Value)) : InterpM Value :=
  match args with
  | [.coroutine v, _] => return v
  | [v, _] => return v
  | [.coroutine v] => return v
  | [v] => return v
  | _ => throwTypeError "asyncio.wait_for() missing arguments"

-- asyncio.ensure_future(coro) → unwrapped value
partial def asyncioEnsureFuture (args : List Value) : InterpM Value :=
  match args with
  | [.coroutine v] => return v
  | [v] => return v
  | _ => throwTypeError "asyncio.ensure_future() takes exactly 1 argument"

end LeanPython.Stdlib.Asyncio
