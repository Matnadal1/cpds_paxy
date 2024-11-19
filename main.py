import re
import select
import subprocess


class Trial:
    sleep = ""
    num_proposers = 1
    num_acceptors = 1
    drop = 0
    delay = 0
    module_version = "1"

    def __init__(self, sleep, num_proposers, num_acceptors, drop, delay, module_version="1"):
        self.sleep = sleep
        self.num_proposers = num_proposers
        self.num_acceptors = num_acceptors
        self.drop = drop
        self.delay = delay
        self.module_version = module_version

    def __str__(self):
        if self.module_version == "1":
            return f"{self.sleep}"
        elif self.module_version == "2":
            return f"{self.sleep}, {self.num_proposers}, {self.num_acceptors}, {self.drop}, {self.delay}"
        return ""


def generate_erlang_command(module, function, params: Trial):
    """
    Generate the Erlang command to execute.
    Args:
        module (str): Erlang module name.
        function (str): Erlang function name.
        params (str): Parameters to pass to the function (as a string).
    Returns:
        str: The Erlang command.
    """
    # Return the Erlang command with the halt instruction
    return f"{module}:{function}({str(params)})."


def open_erlang_shell():
    """
    Open an Erlang shell.
    """
    process = subprocess.Popen(
        ["erl", "-name paxy@127.0.0.1", "-setcookie", "paxy"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    return process


def run_batch(module, function, batch_params, timeout=30):
    """
    Execute batch runs of an Erlang function with specified parameters.
    Args:
        timeout (int): Timeout duration in seconds.
        module (str): Erlang module name.
        function (str): Erlang function name.
        batch_params (list of Trial): List of parameter Trials.
    """
    process = open_erlang_shell()

    # batch_results = []

    for idx, params in enumerate(batch_params):
        command = generate_erlang_command(module, function, params)
        print("====================================================================")
        print(f"Running batch {idx + 1}: {command}")
        try:
            # Execute the Erlang command in bash
            process.stdin.write(command + "\n")
            process.stdin.flush()

            total_time, parsed_results = collect_erlang_responses(process, timeout)

            print(f"\nBatch {idx + 1} completed after {total_time} ms")
            print(parsed_results)
        except Exception as e:
            process.kill()
            print(f"Exception occurred: {e}")
            break
        command = generate_erlang_command(module, "stop", "")
        process.stdin.write(command + "\n")
        process.stdin.flush()
        print("====================================================================")


def collect_erlang_responses(process, timeout):
    responses = []
    while True:
        # Wait for data on process.stdout with a timeout
        ready, _, _ = select.select([process.stdout], [], [], timeout)
        if ready:  # If data is ready
            line = process.stdout.readline()
            if not line or "ok." in line.strip():  # End of stream
                break
            responses.append(line.strip())
        else:
            print(f"Timeout after {timeout} seconds with no new data.")
            break  # Exit the loop on timeout
    return parse_erlang_output(responses)  # Parse the collected lines


def parse_erlang_output(output_lines):
    # Regular expression pattern
    total_time = 0
    pattern = r"\[Proposer (\w+)\] DECIDED (\{[\d,]+\}) in round (\{\d+,\w+\}) after (\d+) ms"
    pattern_time = r"\[Paxy\] Total elapsed time: (\d+) ms"

    parsed_results = []
    for line in output_lines:
        match = re.match(pattern, line)
        if match:
            proposer = match.group(1)
            decision = match.group(2)
            round_info = match.group(3)

            time_taken = int(match.group(4))  # Convert time to integer
            parsed_results.append({
                "proposer": proposer,
                "decision": decision,
                "round": round_info,
                "time_ms": time_taken
            })

        match_time = re.match(pattern_time, line)
        if match_time:
            total_time = int(match_time.group(1))
    return total_time, parsed_results


if __name__ == "__main__":
    # Erlang module and function
    module_name = "paxy2"
    function_name = "start"
    
    # Example batch parameters: [Sleep, NumProposers, NumAcceptors, Drop, Delay]
    # Provide each parameter set as a string in the format you want Erlang to interpret
    batch_parameters = [
        Trial(sleep="[1000, 1200, 1400, 1000, 1000, 10, 50, 500]", num_proposers=3, num_acceptors=5, drop=10, delay=10, module_version="2"),
    ]
    
    run_batch(module_name, function_name, batch_parameters)
