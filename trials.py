from utils import Trial

module_name = "paxy_remote"

TEST_TRIALS = [
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65, 50]", num_proposers=7, num_acceptors=15, drop=1,
              delay=500, prop_timeout=1000, prop_backoff=100, module_version="2"),
]

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

    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=30, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=50, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=70, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=90, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=110, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=150, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=190, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=230, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=270, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=310, prop_timeout=100, prop_backoff=10, module_version="2", count_sorries=True),
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

INCREASING_NUM_ACCEPTORS_TRIALS = [
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=1, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=2, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=4, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=5, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=8, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=10, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=13, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=16, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=19, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=23, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=27, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50]", num_proposers=3, num_acceptors=30, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
]

INCREASING_NUM_PROPOSERS_TRIALS = [
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=1, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=2, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=3, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=4, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=5, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=6, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=7, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=8, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=9, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=10, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
    Trial(module_name, sleep="[10, 100, 10, 20, 50, 10, 80, 65]", num_proposers=11, num_acceptors=3, drop=1,
              delay=10, prop_timeout=100, prop_backoff=10, module_version="2"),
]
