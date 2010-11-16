
-module(smr_SUITE).

-include_lib("eunit/include/eunit.hrl").

%------------------------------------------------------------------------------

basic_whole_system_test() ->
    {ok, Master} = smr_master:start_link(),
    [_ , Host] = string:tokens(atom_to_list(node()), "@"),
    [ok, ok, ok, ok, ok] =
        [smr_master:spawn_worker(Master, list_to_atom(Node ++ "@" ++ Host))
         || Node <- ["test_w1", "test_w2", "test_w3", "test_w4", "test_w5"]],
    lists:foreach(fun (Test) -> whole_system(Test, Master) end,
                  test_suites()).

whole_system({MapFun, ReduceFun, Input, ExpectedResult}, Master) ->
    Result = smr_master:map_reduce(Master, MapFun, ReduceFun, Input),
    ExpectedLength = length(ExpectedResult),
    ?assertMatch(ExpectedLength, length(Result)),
    SortedResult = orddict:to_list(orddict:from_list(Result)),
    ?assertMatch(ExpectedLength, length(SortedResult)),
    SortedExpected = orddict:to_list(orddict:from_list(ExpectedResult)),
    ?assertMatch(ExpectedLength, length(ExpectedResult)),
    lists:zipwith(fun (E, R) -> ?assertMatch(E, R) end,
                  SortedExpected, SortedResult).

test_suites() ->
    [{fun ({_Name, Document}) ->
                 dict:to_list(lists:foldl(
                                  fun (Word, Dict) ->
                                          dict:update_counter(Word, 1, Dict)
                                  end, dict:new(),
                                  string:tokens(Document, " ")))
      end,
      fun ({_Word, CountList}) -> lists:sum(CountList) end,
      [{abc, "Thus the MapReduce framework transforms a list of (key, value) pairs into a list of values. This behavior is different from the functional programming map and reduce combination, which accepts a list of arbitrary values and returns one single value that combines all the values returned by map."},
       {xyz, "MapReduce achieves reliability by parceling out a number of operations on the set of data to each node in the network. Each node is expected to report back periodically with completed work and status updates. If a node falls silent for longer than that interval, the master node (similar to the master server in the Google File System) records the node as dead and sends out the node's assigned work to other nodes. Individual operations use atomic operations for naming file outputs as a check to ensure that there are not parallel conflicting threads running. When files are renamed, it is possible to also copy them to another name in addition to the name of the task (allowing for side-effects)."},
       {def, "A common performance measurement of a network file system is the amount of time needed to satisfy service requests. In conventional systems, this time consists of a disk-access time and a small amount of CPU-processing time. But in a network file system, a remote access has additional overhead due to the distributed structure. This includes the time to deliver the request to a server, the time to deliver the response to the client, and for each direction, a CPU overhead of running the communication protocol software. The performance of a network file system can be viewed as one dimension of its transparency; to be fully equivalent, it would need to be comparable to that of a local disk."}],
      [{"naming", 1}, {"there", 1}, {"framework", 1}, {"The", 1}, {"CPU", 1}, {"work", 2}, {"a", 15}, {"service", 1}, {"viewed", 1}, {"pairs", 1}, {"behavior", 1}, {"server,", 1}, {"System)", 1}, {"system,", 1}, {"map.", 1}, {"combination,", 1}, {"protocol", 1}, {"single", 1}, {"needed", 1}, {"network.", 1}, {"assigned", 1}, {"request", 1}, {"(similar", 1}, {"with", 1}, {"If", 1}, {"expected", 1}, {"combines", 1}, {"and", 6}, {"files", 1}, {"dead", 1}, {"periodically", 1}, {"different", 1}, {"When", 1}, {"use", 1}, {"values", 2}, {"programming", 1}, {"software.", 1}, {"Each", 1}, {"has", 1}, {"reliability", 1}, {"this", 1}, {"would", 1}, {"communication", 1}, {"name", 2}, {"number", 1}, {"longer", 1}, {"records", 1}, {"completed", 1}, {"node's", 1}, {"task", 1}, {"additional", 1}, {"list", 3}, {"systems,", 1}, {"response", 1}, {"access", 1}, {"value", 1}, {"system", 2}, {"File", 1}, {"client,", 1}, {"data", 1}, {"This", 2}, {"functional", 1}, {"values.", 1}, {"disk.", 1}, {"back", 1}, {"also", 1}, {"ensure", 1}, {"sends", 1}, {"arbitrary", 1}, {"local", 1}, {"each", 2}, {"is", 4}, {"remote", 1}, {"be", 3}, {"can", 1}, {"falls", 1}, {"direction,", 1}, {"file", 4}, {"accepts", 1}, {"threads", 1}, {"dimension", 1}, {"all", 1}, {"Individual", 1}, {"report", 1}, {"In", 1}, {"copy", 1}, {"comparable", 1}, {"node", 5}, {"another", 1}, {"status", 1}, {"in", 4}, {"operations", 3}, {"equivalent,", 1}, {"other", 1}, {"transparency;", 1}, {"conventional", 1}, {"returns", 1}, {"to", 17}, {"MapReduce", 2}, {"them", 1}, {"side-effects).", 1}, {"addition", 1}, {"structure.", 1}, {"it", 2}, {"possible", 1}, {"updates.", 1}, {"server", 1}, {"performance", 2}, {"Google", 1}, {"conflicting", 1}, {"satisfy", 1}, {"check", 1}, {"interval,", 1}, {"outputs", 1}, {"into", 1}, {"parallel", 1}, {"parceling", 1}, {"which", 1}, {"returned", 1}, {"common", 1}, {"disk-access", 1}, {"are", 2}, {"value)", 1}, {"the", 20}, {"A", 1}, {"atomic", 1}, {"amount", 2}, {"includes", 1}, {"CPU-processing", 1}, {"time.", 1}, {"renamed,", 1}, {"distributed", 1}, {"due", 1}, {"not", 1}, {"Thus", 1}, {"one", 2}, {"fully", 1}, {"set", 1}, {"silent", 1}, {"network", 3}, {"of", 14}, {"its", 1}, {"achieves", 1}, {"But", 1}, {"map", 1}, {"consists", 1}, {"running", 1}, {"as", 3}, {"for", 4}, {"nodes.", 1}, {"(allowing", 1}, {"on", 1}, {"by", 2}, {"measurement", 1}, {"small", 1}, {"need", 1}, {"master", 2}, {"time", 5}, {"requests.", 1}, {"(key,", 1}, {"reduce", 1}, {"out", 2}, {"deliver", 2}, {"than", 1}, {"overhead", 2}, {"that", 4}, {"from", 1}, {"transforms", 1}, {"running.", 1}]},
     
     {fun ({K, V}) -> [{keys, K}, {values, V}] end,
      fun ({keys, Ks}) -> lists:sum(Ks);
          ({values, Vs}) -> lists:sum(Vs)
      end,
      [{K, V} || K <- lists:seq(1, 50), V <- lists:seq(40, 1, -1)],
      [{keys, 40 * 50 * 51 div 2}, {values, 50 * 40 * 41 div 2}]}
    ].
