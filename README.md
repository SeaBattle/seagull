# SeaConfig
SeaConfig service. Submodule for working with configuration in other sea services.

## Usage
For configuration use `sys.config`:

    {seaconfig, 
        [
            {backend, {Backend, BackendUrl}}
        ]
    }
Where:  
__Backend__ is one of the two supported Backends: `consul` or `etcd`.  
__BackendUrl__ is url to reach the service.  
Example:

    {seaconfig, 
            [
                {backend, {consul, "http://127.0.0.1:8500/"}}
            ]
        }
### Caching kv
You can cache kv storage in ets by adding `{cache, [{enable, true}]}` to 
`sys.config`. By default cache is `false`. Also you can add update interval
for cache values to be synchronized with backend by adding 
`{update_time, TimeMS}` to `cache` section.  
Full Example:

    {seaconfig, 
            [
                {backend, {consul, "http://127.0.0.1:8500/"}},
                {cache, [{enable, true}, {update_time, 15000}]}
            ]
        }
### On update callbacks
If you enabled cache update - you can add callback function, which will 
be called, when value is updated.  
Remember, that callbacks are synchronous and are called from main conf sycle,
so do not do any long operations in them.  
Example:
    
    ResetAllPassFun = fun(NewPass) -> database_man ! {reset_pass, NewPass} end, 
    seaconfig:add_callback(<<"user_pass">>, ResetAllPassFun).
Callback can be removed with `remove_callback:/1` function.