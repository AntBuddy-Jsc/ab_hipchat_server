{application,worker_pool,
             [{description,"Erlang Worker Pool"},
              {vsn,"1.0.3"},
              {id,"worker_pool"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{wpool,[]}},
              {env,[]},
              {maintainers,["Inaka","TigerText"]},
              {licenses,["Apache2"]},
              {links,[{"Github","https://github.com/inaka/worker_pool"},
                      {"Erldocs","http://inaka.github.io/worker_pool/"},
                      {"Blog Post",
                       "http://inaka.net/blog/2014/09/25/worker-pool/"}]},
              {modules,[echo_server,wpool,wpool_SUITE,wpool_bench,wpool_pool,
                        wpool_pool_SUITE,wpool_process,wpool_process_SUITE,
                        wpool_process_sup,wpool_queue_manager,wpool_shutdown,
                        wpool_sup,wpool_time_checker,wpool_worker,
                        wpool_worker_SUITE]}]}.