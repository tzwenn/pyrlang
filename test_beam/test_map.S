{module, test_map}.  %% version = 0

{exports, [{module_info,0},{module_info,1},{test,0},{test_map,1}]}.

{attributes, []}.

{labels, 11}.


{function, test_map, 1, 2}.
  {label,1}.
    {line,[{location,"test_map.erl",4}]}.
    {func_info,{atom,test_map},{atom,test_map},1}.
  {label,2}.
    {make_fun2,{f,10},0,0,1}.
    {move,{literal,[1]},{x,1}}.
    {line,[{location,"test_map.erl",5}]}.
    {call_ext_only,2,{extfunc,lists,map,2}}.


{function, test, 0, 4}.
  {label,3}.
    {line,[{location,"test_map.erl",8}]}.
    {func_info,{atom,test_map},{atom,test},0}.
  {label,4}.
    {move,{integer,3},{x,0}}.
    {call_only,1,{f,2}}.


{function, module_info, 0, 6}.
  {label,5}.
    {line,[]}.
    {func_info,{atom,test_map},{atom,module_info},0}.
  {label,6}.
    {move,{atom,test_map},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,test_map},{atom,module_info},1}.
  {label,8}.
    {move,{x,0},{x,1}}.
    {move,{atom,test_map},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-test_map/1-fun-0-', 2, 10}.
  {label,9}.
    {line,[{location,"test_map.erl",5}]}.
    {func_info,{atom,test_map},{atom,'-test_map/1-fun-0-'},2}.
  {label,10}.
    {line,[{location,"test_map.erl",6}]}.
    {gc_bif,'+',{f,0},2,[{x,0},{x,1}],{x,0}}.
    return.
