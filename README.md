# Erlang Batch Run Python Script

## Overview
This project contains a Python script to execute batch runs for Erlang modules, specifically for running `paxy_remote` modules. The script automates the interaction with Erlang nodes to run a series of trials defined in the Python code.

## Requirements
The following Python packages are required for the script to work. You can install them using `pip`:

```sh
pip install -r requirements.txt
```

The dependencies are listed in `requirements.txt` and include libraries such as:
- `pexpect` for interacting with the Erlang shell.
- `matplotlib` and `numpy` for plotting and data analysis.

## Setup
You need to launch two Erlang nodes before running the Python script. Use the following commands:

```sh
erl -name paxy-acc@127.0.0.1 -setcookie paxos
erl -name paxy-pro@127.0.0.1 -setcookie paxos
```
These nodes are required for running the `paxy_remote` Erlang module, which is essential for simulating a distributed system.

**Note**: Make sure that both Erlang nodes are run in the same directory as the Erlang files. Additionally, all `.erl` files need to be compiled using the Erlang compiler before running the nodes.

## Running the Batch Script
After setting up the Erlang nodes, you can run the batch script to execute the trials:

```sh
python main.py
```
Alternatively, if you want to run the batch directly using Erlang without Python, you can use the following command:

```sh
erl -name paxy-run@127.0.0.1 -setcookie paxos -eval 'paxy_remote:start(NumProposers, NumAcceptors, DropRate, Delay, ProposerTimeout, ProposerBackoff, ModuleVersion)'
```
Replace the parameters accordingly:
- **`NumProposers`**: The number of proposers.
- **`NumAcceptors`**: The number of acceptors.
- **`DropRate`**: The drop rate to simulate packet loss.
- **`Delay`**: The delay for acceptors.
- **`ProposerTimeout`**: Timeout duration for proposers.
- **`ProposerBackoff`**: Backoff time for proposers in case of retries.
- **`ModuleVersion`**: Version of the Erlang module (`1` or `2`).
The script will execute several trials using Erlang's functionality. Each trial is defined in the `trials.py` file, and they represent different configurations for proposers, acceptors, delays, and other parameters. The results are saved in an output CSV file located in the `out` directory.

### Key Files
- **`main.py`**: This is the main file where you execute the trials. It handles setting up the Erlang instances and collects responses.
- **`trials.py`**: Defines the different trials to be executed during the batch run. Each trial specifies various parameters like the number of proposers and acceptors.
- **`modular_acceptor.erl`**: The Erlang module that implements the acceptor logic.
- **`modular_proposer.erl`**: The Erlang module that implements the proposer logic.
- **`paxy_remote.erl`**: The Erlang module that orchestrates the Paxos-based distributed consensus across multiple instances.
- **`paxy_local.erl`**: The Erlang module similar to `paxy_remote`, but it runs everything on a single instance instead of multiple instances. Both `paxy_remote` and `paxy_local` require context input and work with the `modular_acceptor` and `modular_proposer` modules to achieve distributed consensus.
- **`process.py`**: Handles the collection of responses from the Erlang nodes using regex patterns to parse decisions, times, and rounds from the output.
- **`requirements.txt`**: Contains all the Python dependencies needed to run the script.

### How It Works
1. **Initialize Erlang Nodes**: Before running the Python script, two Erlang nodes are launched with a given cookie (`paxos`). These nodes are the backend of the consensus mechanism.
2. **Run Batch Trials**: The `run_batch()` function in `main.py` runs a series of batch trials as defined in `trials.py`. It sends commands to the Erlang nodes and listens for responses. 
3. **Collect Responses**: Data such as elapsed time and decision rounds are collected and optionally plotted as histograms. The results are saved in a CSV file located in the `out` directory for further analysis.

## Creating a Batch in `trials.py`
To create a batch of trials in the `trials.py` file, you can define a list of `Trial` instances with different configurations. A `Trial` is an object that encapsulates various parameters like the number of proposers, number of acceptors, delay, and more. Here is an example:

```python
from utils import Trial

module_name = "paxy_remote"

NEW_BATCH_TRIALS = [
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=5, num_acceptors=10, drop=2,
          delay=100, prop_timeout=500, prop_backoff=50, module_version="2"),
    Trial(module_name, sleep="[20, 50, 30, 60, 80]", num_proposers=3, num_acceptors=8, drop=1,
          delay=150, prop_timeout=600, prop_backoff=30, module_version="2"),
    Trial(module_name, sleep="[15, 30, 25, 40, 55]", num_proposers=7, num_acceptors=12, drop=3,
          delay=200, prop_timeout=700, prop_backoff=40, module_version="2"),
]
```

### Parameters Explained
- **`module_name`**: The name of the Erlang module to use (`paxy_remote` in this case).
- **`sleep`**: A string specifying sleep times for various stages of the trial.
- **`num_proposers`**: Number of proposers involved in the consensus.
- **`num_acceptors`**: Number of acceptors involved in the consensus.
- **`drop`**: Drop rate (e.g., packet loss) that you want to simulate.
- **`delay`**: Delay time for acceptors.
- **`prop_timeout`**: Timeout for proposers.
- **`prop_backoff`**: Backoff time for proposers in case of retries.
- **`module_version`**: Version of the Erlang module.

You can then add these new batch trials to the main script for execution by importing them and including them in the trial list:

```python
from trials import NEW_BATCH_TRIALS

TRIALS = NEW_BATCH_TRIALS
```
This allows you to customize the trials based on your needs, and the script will execute each of these trials sequentially.

## Customization
- Modify `trials.py` to change the configurations and add new trials.

## Example
Here is an example to get you started:
1. Start Erlang nodes:
   ```sh
   erl -name paxy-acc@127.0.0.1 -setcookie paxos
   erl -name paxy-pro@127.0.0.1 -setcookie paxos
   ```
2. Run the batch trials:
   ```sh
   python main.py
   ```
   This will run a set of predefined trials and save the results to a CSV file in the `out` directory.

## Troubleshooting
- **Timeouts**: If the script times out during a trial, increase the `timeout` parameter in the `run_batch()` function.
- **Node Communication Issues**: Ensure that both Erlang nodes are running and using the same cookie (`paxos`). Additionally, make sure the nodes are run in the same directory where the Erlang files are located, and that all `.erl` files are compiled beforehand.
