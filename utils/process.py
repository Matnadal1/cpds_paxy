import subprocess

import pexpect

decision_pattern = r"\[Proposer (\w+)\] DECIDED (\{[\d,]+\}) in round (\{\d+,\w+\}) after (\d+) ms"
time_pattern = r"\[Paxy\] Total elapsed time: (\d+) ms"
done_pattern = r"ok."


def open_erlang_shell():
    """
    Open an Erlang shell.
    """
    process = subprocess.Popen(
        ["erl", "-name paxy2@127.0.0.1", "-setcookie", "paxy"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    return process


def collect_erlang_responses(process, timeout):
    total_time = 0
    max_round = 0
    parsed_results = []
    timeout_end = False
    while True:
        # print(process.read().decode('utf-8'), end="", flush=True)
        match_idx = process.expect([decision_pattern, time_pattern, pexpect.EOF, pexpect.TIMEOUT], timeout=timeout)
        if match_idx == 0:
            proposer = process.match.group(1)
            decision = process.match.group(2)
            round_info = (str(process.match.group(3)).
                          replace("{", "").
                          replace("}", "").
                          replace("'", "").
                          replace("b", "").
                          split(","))

            time_taken = int(process.match.group(4))  # Convert time to integer
            parsed_results.append({
                "proposer": proposer,
                "decision": decision,
                "round": int(round_info[0]),
                "value_of_acceptor": round_info[1],
                "time_ms": time_taken
            })
            max_round = max(max_round, int(round_info[0]))
            total_time += time_taken
        elif match_idx == 1:
            total_time = int(process.match.group(1))
            break
        elif match_idx == 2:
            break
        elif match_idx == 3:
            timeout_end = True
            print(f"Timeout after {timeout} seconds with no new data.")
            break  # Exit the loop on timeout

    return total_time, parsed_results, max_round, timeout_end
