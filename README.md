# SeaConfig
Configuration api using with `etcd` and `consul` for service-discovery and
key-value in microservices.

## Configuration
### Static
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
                {backend, {consul, "http://127.0.0.1:8500"}}
            ]
        }
### Dynamic
In case you don't know consul url on compilation time you do not need to specify seaconfig 
configuration in `sys.config`. Just when you obtain you module, url and other params call 
`seaconfig:add_backend/2/3`. Third argument is a proplist with all your options (listed below).  
Example:  

    seaconfig:add_backend(consul, 172.0.0.2, [{cache, [{enable, true}, {update_time, 15000}]}]).
### Caching kv
You can cache kv storage in ets by adding `{cache, [{enable, true}]}` to 
`sys.config`. By default cache is `false`. Also you can add update interval
for cache values to be synchronized with backend by adding 
`{update_time, TimeMS}` to `cache` section.  
Full Example:

    {seaconfig, 
            [
                {backend, {consul, "http://127.0.0.1:8500"}},
                {cache, [{enable, true}, {update_time, 15000}]}
            ]
        }
### Service auto registration
To make registration of your service on application start - add `seaconfig` to
your applications and add this option to seaconfig conf in `sys.config`:

    {seaconfig, 
            [
                ...
                {autoregister, #{service => Service, address => Address, port => Port}},
                ...
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

## Usage
### Unified api
After configuring backend you can use unified api:

    1> seaconfig:get_services().
    #{<<"consul">> => [],<<"postgres">> => [],<<"redis">> => []}

### Consul special api
You can use consul special api with consul backend:

    1> seaconfig:dns_request("redis").
    [{1,1,6379,"tihon_home.node.dc1.consul"},
    {1,1,6379,"tihon_work.su.node.dc1.consul"}]
    2> seaconfig:get_service_near("redis").
    {ok, #{<<"Address">> => <<"192.168.1.204">>,
            <<"CreateIndex">> => 313,
            <<"ModifyIndex">> => 313,
            <<"Node">> => <<"tihon_work.su">>,
            <<"ServiceAddress">> => <<>>,
            <<"ServiceEnableTagOverride">> => false,
            <<"ServiceID">> => <<"tihon_work.su:redis:6379">>,
            <<"ServiceName">> => <<"redis">>,
            <<"ServicePort">> => 6379,
            <<"ServiceTags">> => [],
            <<"TaggedAddresses">> => #{<<"wan">> => <<"192.168.1.204">>}}, 
            [#{<<"Address">> => <<"192.168.1.105">>,
                <<"CreateIndex">> => 231,
                <<"ModifyIndex">> => 231,
                <<"Node">> => <<"tihon_home">>,
                <<"ServiceAddress">> => <<>>,
                <<"ServiceEnableTagOverride">> => false,
                <<"ServiceID">> => <<"tihon_home:redis:6379">>,
                <<"ServiceName">> => <<"redis">>,
                <<"ServicePort">> => 6379,
                <<"ServiceTags">> => [],
                <<"TaggedAddresses">> => #{<<"lan">> => <<"192.168.1.105">>,
                    <<"wan">> => <<"192.168.1.105">>}}]