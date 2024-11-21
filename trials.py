from utils import Trial

module_name = "paxy2"

INCREASING_DELAY_TRIALS = [
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=30, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=50, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=70, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=90, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=110, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=150, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=190, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=230, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=270, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=310, prop_timeout=100, prop_backoff=10, module_version="2"),
]

INCREASING_DROP_TRIALS = [
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,  # 0
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=5,  # 1
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=10,  # 2
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=15,  # 3
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=20,  # 4
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=25,  # 5
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=30,  # 6
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=35,  # 7
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=40,  # 8
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=45,  # 9
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=50,  # 10
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=55,  # 11
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=60,  # 12
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=65,  # 13
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=70,  # 14
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
]
