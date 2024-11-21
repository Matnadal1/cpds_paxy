class ErlangCommand:

    def __init__(self, module, function, params):
        self.module = module
        self.function = function
        self.params = params

    def __str__(self):
        return f"{self.module}:{self.function}({self.params})."


class Trial(ErlangCommand):
    sleep = ""
    num_proposers = 1
    num_acceptors = 1
    drop = 0
    acceptors_delay = 0
    proposers_timeout = 100
    proposers_backoff = 10
    module_version = "1"

    def __init__(self, module, sleep, num_proposers, num_acceptors, drop, delay, prop_timeout, prop_backoff, module_version="1", function="start"):
        self.sleep = sleep
        self.num_proposers = num_proposers
        self.num_acceptors = num_acceptors
        self.drop = drop
        self.acceptors_delay = delay
        self.module_version = module_version
        self.proposers_timeout = prop_timeout
        self.proposers_backoff = prop_backoff

        self.params = ""
        if self.module_version == "1":
            self.params = f"{self.sleep}"
        elif self.module_version == "2":
            self.params = f"{self.sleep}, {self.num_proposers}, {self.num_acceptors}, {self.drop}, {self.acceptors_delay}, {self.proposers_timeout}, {self.proposers_backoff}"
        super().__init__(module, function, self.params)
