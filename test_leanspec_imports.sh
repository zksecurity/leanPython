#!/bin/bash
# leanSpec module import coverage test.
# Attempts to import each leanSpec source module through the LeanPython interpreter.
# Reports PASS / XFAIL (expected failure with issue#) / FAIL (unexpected).
#
# Usage: ./test_leanspec_imports.sh

set -euo pipefail

EXE=".lake/build/bin/leanPython"
SRC="/home/zksecurity/leanPython/references/leanSpec/src"

if [ ! -f "$EXE" ]; then
  echo "error: $EXE not found — run 'lake build' first"
  exit 1
fi

PASS=0
FAIL=0
XFAIL=0
FAILURES=""

# Expected failures: module -> "issue_number reason"
# Modules matching these entries are XFAIL (expected to fail).
# Uses a lookup function instead of `declare -A` for Bash 3.2 (macOS) compatibility.
xfail_reason() {
  case "$1" in
    # #5: lean_multisig_py (pending pure Python impl)
    lean_spec.subspecs.xmss.aggregation) echo "#5 lean_multisig_py" ;;
    # Transitive: everything importing xmss.aggregation
    lean_spec.subspecs.xmss.interface) echo "#5 imports aggregation transitively" ;;
    lean_spec.subspecs.containers.attestation.attestation) echo "#5 imports xmss.aggregation" ;;
    lean_spec.subspecs.containers.block.block) echo "#5 imports xmss.aggregation" ;;
    lean_spec.subspecs.containers.block.types) echo "#4,#5 dict subclass + xmss.aggregation" ;;
    lean_spec.subspecs.containers.state.state) echo "#5 imports xmss.aggregation" ;;
    lean_spec.subspecs.containers.validator) echo "#5 imports xmss.containers->config" ;;
    lean_spec.subspecs.containers.attestation.aggregation_bits) echo "#5 imports validator->xmss" ;;
    lean_spec.subspecs.containers.state.types) echo "#5 imports validator->xmss" ;;
    # #5: XMSS modules that need lean_spec.config (get_args / PEP 695 __value__)
    lean_spec.config) echo "#5 get_args + PEP695 __value__" ;;
    lean_spec.subspecs.xmss.constants) echo "#5 imports lean_spec.config" ;;
    lean_spec.subspecs.xmss.types) echo "#5 imports xmss.constants" ;;
    lean_spec.subspecs.xmss.containers) echo "#5 imports xmss.constants" ;;
    lean_spec.subspecs.xmss._validation) echo "#5 imports lean_spec.config" ;;
    lean_spec.subspecs.xmss.hypercube) echo "#5 imports xmss.constants" ;;
    lean_spec.subspecs.xmss.message_hash) echo "#5 imports xmss chain" ;;
    lean_spec.subspecs.xmss.poseidon) echo "#5 imports xmss chain" ;;
    lean_spec.subspecs.xmss.prf) echo "#5 imports xmss.constants" ;;
    lean_spec.subspecs.xmss.rand) echo "#5 imports xmss.constants" ;;
    lean_spec.subspecs.xmss.subtree) echo "#5 imports xmss.types" ;;
    lean_spec.subspecs.xmss.target_sum) echo "#5 imports xmss.constants" ;;
    lean_spec.subspecs.xmss.tweak_hash) echo "#5 imports xmss chain" ;;
    lean_spec.subspecs.xmss.utils) echo "#5 imports xmss chain" ;;
    # #6: numpy/numba (pending pure Python impl)
    lean_spec.subspecs.poseidon2.permutation) echo "#6 numpy" ;;
    lean_spec.subspecs.poseidon2.constants) echo "#6 imports permutation transitively" ;;
    # #7: asyncio
    lean_spec.__main__) echo "#7 asyncio" ;;
    lean_spec.subspecs.chain.clock) echo "#7 asyncio" ;;
    lean_spec.subspecs.chain.service) echo "#7 asyncio" ;;
    lean_spec.subspecs.api.server) echo "#7 asyncio" ;;
    lean_spec.subspecs.api.routes) echo "#7 imports api.server" ;;
    lean_spec.subspecs.api.endpoints.checkpoints) echo "#7 imports api chain" ;;
    lean_spec.subspecs.api.endpoints.fork_choice) echo "#7 imports api chain" ;;
    lean_spec.subspecs.api.endpoints.health) echo "#7 imports api chain" ;;
    lean_spec.subspecs.api.endpoints.metrics) echo "#7 imports api chain" ;;
    lean_spec.subspecs.api.endpoints.states) echo "#7 asyncio" ;;
    lean_spec.subspecs.networking.client.event_source) echo "#7 asyncio" ;;
    lean_spec.subspecs.networking.client.reqresp_client) echo "#7 asyncio" ;;
    lean_spec.subspecs.networking.discovery.service) echo "#7 asyncio" ;;
    lean_spec.subspecs.networking.discovery.transport) echo "#7 asyncio + cryptography" ;;
    lean_spec.subspecs.networking.gossipsub.behavior) echo "#7 asyncio" ;;
    lean_spec.subspecs.networking.service.service) echo "#7 asyncio" ;;
    lean_spec.subspecs.networking.service.events) echo "#7 imports service chain" ;;
    lean_spec.subspecs.networking.transport.quic.connection) echo "#7 asyncio + aioquic" ;;
    lean_spec.subspecs.networking.transport.quic.stream_adapter) echo "#7 asyncio" ;;
    lean_spec.subspecs.node.node) echo "#7 asyncio" ;;
    lean_spec.subspecs.sync.service) echo "#7 asyncio" ;;
    lean_spec.subspecs.sync.backfill_sync) echo "#7 imports sync chain" ;;
    lean_spec.subspecs.sync.checkpoint_sync) echo "#7 imports sync chain" ;;
    lean_spec.subspecs.sync.head_sync) echo "#7 imports sync chain" ;;
    lean_spec.subspecs.validator.service) echo "#7 asyncio" ;;
    lean_spec.subspecs.validator.registry) echo "#5,#7 imports xmss + chain" ;;
    # chain/__init__.py imports clock.py which uses asyncio, blocking all chain.* imports
    lean_spec.subspecs.chain.config) echo "#7 chain/__init__.py imports clock->asyncio" ;;
    lean_spec.subspecs.containers.checkpoint) echo "#7 imports chain->clock->asyncio" ;;
    lean_spec.subspecs.containers.config) echo "#7 imports chain->clock->asyncio" ;;
    lean_spec.subspecs.containers.slot) echo "#7 imports chain->clock->asyncio" ;;
    # #9: prometheus_client
    lean_spec.subspecs.metrics.registry) echo "#9 prometheus_client" ;;
    lean_spec.subspecs.forkchoice.store) echo "#5,#9 xmss + prometheus_client" ;;
    # Other external deps (cryptography, aioquic, sqlite3)
    lean_spec.subspecs.networking.discovery.crypto) echo "cryptography lib" ;;
    lean_spec.subspecs.networking.discovery.keys) echo "cryptography lib" ;;
    lean_spec.subspecs.networking.discovery.handshake) echo "cryptography lib" ;;
    lean_spec.subspecs.networking.discovery.codec) echo "imports discovery chain" ;;
    lean_spec.subspecs.networking.discovery.config) echo "imports discovery chain" ;;
    lean_spec.subspecs.networking.discovery.messages) echo "imports discovery chain" ;;
    lean_spec.subspecs.networking.discovery.packet) echo "imports discovery chain" ;;
    lean_spec.subspecs.networking.discovery.routing) echo "imports discovery chain" ;;
    lean_spec.subspecs.networking.discovery.session) echo "imports discovery chain" ;;
    lean_spec.subspecs.networking.enr.enr) echo "cryptography lib" ;;
    lean_spec.subspecs.networking.enr.eth2) echo "imports enr chain" ;;
    lean_spec.subspecs.networking.enr.keys) echo "cryptography lib" ;;
    lean_spec.subspecs.networking.transport.identity.keypair) echo "cryptography lib" ;;
    lean_spec.subspecs.networking.transport.identity.signature) echo "cryptography lib" ;;
    lean_spec.subspecs.networking.transport.peer_id) echo "imports identity chain" ;;
    lean_spec.subspecs.networking.transport.protocols) echo "imports transport chain" ;;
    lean_spec.subspecs.networking.transport.quic.tls) echo "aioquic + cryptography" ;;
    lean_spec.subspecs.networking.peer) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.config) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.types) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.varint) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.reqresp.codec) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.reqresp.handler) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.reqresp.message) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.gossipsub.mcache) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.gossipsub.mesh) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.gossipsub.message) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.gossipsub.parameters) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.gossipsub.rpc) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.gossipsub.topic) echo "imports networking chain" ;;
    lean_spec.subspecs.networking.gossipsub.types) echo "imports networking chain" ;;
    lean_spec.subspecs.storage.sqlite) echo "sqlite3" ;;
    lean_spec.subspecs.storage.database) echo "imports storage chain" ;;
    lean_spec.subspecs.storage.exceptions) echo "imports storage chain" ;;
    lean_spec.subspecs.storage.namespaces) echo "imports storage chain" ;;
    lean_spec.subspecs.sync.block_cache) echo "imports sync chain" ;;
    lean_spec.subspecs.sync.config) echo "imports sync chain" ;;
    lean_spec.subspecs.sync.peer_manager) echo "imports sync chain" ;;
    lean_spec.subspecs.sync.states) echo "imports sync chain" ;;
    lean_spec.subspecs.genesis.config) echo "#5 imports xmss chain" ;;
    lean_spec.subspecs.genesis.state) echo "#5 imports genesis.config" ;;
    *) echo "" ;;
  esac
}

# --- Test runner ---

MAIN_PY="$SRC/_leanpython_import_test.py"
cleanup() { rm -f "$MAIN_PY"; }
trap cleanup EXIT

try_import() {
  local mod="$1"

  cat > "$MAIN_PY" << PYEOF
import $mod
PYEOF

  local output
  output=$(timeout 30 "$EXE" "$MAIN_PY" 2>&1) || true

  if [ -z "$output" ]; then
    return 0  # success (no output = no error)
  else
    echo "$output"
    return 1
  fi
}

echo "=== LeanPython leanSpec import coverage ==="
echo ""

# Collect all modules
MODULES=$(find "$SRC/lean_spec" -name "*.py" -not -name "__init__.py" -not -path "*__pycache__*" \
  | sed "s|$SRC/||; s|/|.|g; s|\.py$||" | sort)

for mod in $MODULES; do
  xfail_reason="$(xfail_reason "$mod")"

  output=$(try_import "$mod" 2>&1) && ok=true || ok=false

  if $ok; then
    if [ -n "$xfail_reason" ]; then
      # Unexpectedly passed — still good, report as PASS
      printf "  PASS  %-60s (was xfail: %s)\n" "$mod" "$xfail_reason"
    else
      printf "  PASS  %s\n" "$mod"
    fi
    PASS=$((PASS + 1))
  else
    if [ -n "$xfail_reason" ]; then
      printf "  XFAIL %-60s [%s]\n" "$mod" "$xfail_reason"
      XFAIL=$((XFAIL + 1))
    else
      printf "  FAIL  %s\n" "$mod"
      FAILURES="$FAILURES\n  $mod: $(echo "$output" | head -1)"
      FAIL=$((FAIL + 1))
    fi
  fi
done

echo ""
echo "Results: $PASS pass, $FAIL fail, $XFAIL xfail (expected)"

if [ $FAIL -gt 0 ]; then
  echo ""
  echo "Unexpected failures:"
  printf "$FAILURES\n"
  exit 1
fi

exit 0
