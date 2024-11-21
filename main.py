import csv
import signal
import time

from pexpect.popen_spawn import PopenSpawn

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import MaxNLocator

from utils import Trial, ErlangCommand, collect_erlang_responses, Plotter
from trials import *

# Trial(module_name, sleep, num_proposers, num_acceptors, drop, delay, prop_timeout, prop_backoff)
TRIALS = INCREASING_NUM_ACCEPTORS_TRIALS
OUTPUT_NAME = "increasing_acceptors_30"

NEW_TRIALS = True
FILENAME = f"out/paxy3_no_sorry_increasing_proposers_1732215926.304058.csv"


def run_batch(module, batch_trials, timeout=30, n=1):
    """
    Execute batch runs of an Erlang function with specified parameters.
    Args:
        timeout (int): Timeout duration in seconds.
        module (str): Erlang module name.
        batch_trials (list of Trial): List of parameter Trials.
    """
    for idx, cmd in enumerate(batch_trials):
        print("====================================================================")
        print(f"Running batch {idx + 1}: {str(cmd)}")

        process = PopenSpawn(["erl", "-name", f"paxy2-{idx}@127.0.0.1", "-setcookie", "paxos"])
        total_time = 0
        trial_time_results = []
        trial_round_results = []
        for _ in range(n):
            # Execute the Erlang command in bash
            process.write(str(cmd) + "\n")

            trial_time, parsed_results, max_round, timeout_end = collect_erlang_responses(process, timeout)
            total_time += trial_time

            trial_time_results.append(trial_time)
            if not timeout_end:
                trial_round_results.append(max_round)
            else:
                trial_round_results.append(max_round)
                print(f"Timeout occurred in batch {idx + 1} after {max_round} rounds.")

            command = str(ErlangCommand(module, "stopAll", ""))
            process.write(command + "\n")
            time.sleep(0.1)
        process.kill(signal.SIGTERM)
        print(f"\nBatch {idx + 1} completed after {total_time} ms")
        print("====================================================================")
        yield total_time, trial_time_results, trial_round_results


if __name__ == "__main__":

    if NEW_TRIALS:
        num_proposers = []
        num_acceptors = []
        drop_percentages = []
        delays = []
        rounds = []
        timeouts = []
        backoffs = []
        elapsed_times = []
        total_times = []
        for i, (trial_input, (total_time, trial_time_results, trial_round_results)) in enumerate(zip(
                TRIALS, run_batch(module_name, TRIALS, timeout=10, n=20))):
            try:
                fig, axs = plt.subplots(2, figsize=(8, 6))

                num_proposers.append(trial_input.num_proposers)
                num_acceptors.append(trial_input.num_acceptors)
                delays.append(trial_input.acceptors_delay)
                drop_percentages.append(trial_input.drop)
                timeouts.append(trial_input.proposers_timeout)
                backoffs.append(trial_input.proposers_backoff)

                rounds.append(np.average(trial_round_results))
                elapsed_times.append(np.average(trial_time_results))
                total_times.append(total_time)

                axs[0].hist(trial_time_results, bins=10, color='b', alpha=0.5)
                axs[0].set_xlim(min(trial_time_results), max(trial_time_results))
                axs[0].set_xlabel('Time taken (ms)')
                axs[0].set_ylabel('Frequency')

                axs[1].hist(trial_round_results, bins=10, color='r', alpha=0.5)
                axs[1].xaxis.set_major_locator(MaxNLocator(integer=True))
                axs[1].set_xlabel('Round')
                axs[1].set_ylabel('Frequency')

                fig.suptitle(
                    f"Trial {i + 1} (delay={trial_input.acceptors_delay}, drop={trial_input.drop}) "
                    f"Results: Time Taken and Round Frequency",
                    fontsize=14,
                )
                fig.tight_layout()
                plt.show()
            except KeyboardInterrupt as e:
                print("Interrupted by user.")
                break

        FILENAME = f"out/{module_name}_{OUTPUT_NAME}_{time.time()}.csv"
        with open(FILENAME, "w") as f:
            writer = csv.writer(f)
            writer.writerow(["num_proposers", "num_acceptors", "drop_percentage", "delay", "timeouts", "backoffs", "relative_delay", "rounds", "elapsed_times", "total_times"])
            for i_num_proposers, i_num_acceptors, i_drop_percentage, i_delay, i_timeouts, i_backoffs, i_rounds, i_elapsed_times, i_total_times in zip(
                    num_proposers, num_acceptors, drop_percentages, delays, timeouts, backoffs, rounds, elapsed_times, total_times):
                writer.writerow(
                    [
                        i_num_proposers,
                        i_num_acceptors,
                        i_drop_percentage,
                        i_delay,
                        i_timeouts,
                        i_backoffs,
                        (i_delay/i_timeouts),
                        i_rounds,
                        i_elapsed_times,
                        i_total_times,
                    ]
                )

    plotter = Plotter(FILENAME)
    plotter.plot("num_acceptors", "rounds", "Acceptors vs Rounds")
    plotter.plot("num_acceptors", "elapsed_times", "Acceptors vs Consensus Time")
    # plotter.plot_n_lines("sorry", "Proposers vs Rounds", "num_proposers", "rounds", ["Not Sorry", "Sorry"])
    # plotter.plot_n_lines("sorry", "Proposers vs Consensus Time", "num_proposers", "elapsed_times", ["Not Sorry", "Sorry"])
