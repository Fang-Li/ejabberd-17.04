-module(test_ejabberd).
-compile(export_all).
-include("logger.hrl").


make_offline_list(N) ->
  make_offline_list(N,Acc=[]).
make_offline_list(1,Acc) ->
  Timestamp = erlang:integer_to_binary(timestamp()),
  <<MsgTime:13/binary,_/binary>> = Timestamp,
  
  [{obj,[{"msgid",Timestamp},
              {"id",Timestamp},
              {"msgTime",MsgTime},
              {"mask",<<"0">>},
              {"userid",<<"15475">>},
              {"username",<<"leon">>},
              {"userimage",
               <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200_200_2_80.jpg">>},
              {"usergender",1},
              {"type",1000},
              {"content",erlang:integer_to_binary(1)},
              {"msgsource",<<"XX">>},
              {"usersource",<<"yuejian.net">>},
              {"userfrom",<<"XX">>},
              {"msgtype",<<"0">>}]} | Acc];
make_offline_list(N,Acc) ->
  Timestamp = erlang:integer_to_binary(timestamp(N,N)),
  <<MsgTime:13/binary,_/binary>> = Timestamp,
  
  Acc2 = [{obj,[{"msgid",Timestamp},
              {"id",Timestamp},
              {"msgTime",MsgTime},
              {"mask",<<"0">>},
              {"userid",<<"15475">>},
              {"username",<<"leon">>},
              {"userimage",
               <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200_200_2_80.jpg">>},
              {"usergender",1},
              {"type",1000},
              {"content",erlang:integer_to_binary(N)},
              {"msgsource",<<"XX">>},
              {"usersource",<<"yuejian.net">>},
              {"userfrom",<<"XX">>},
              {"msgtype",<<"0">>}]} | Acc],
  make_offline_list(N-1,Acc2).

  
  
route(N) ->

  OfflineList = make_offline_list(N),

  CData = rfc4627:encode({obj,[{"mask",<<"0">>},
      {"offlinelist",OfflineList},
      {"type",<<"10000">>},
      {"groupimage",
       <<"http://beta.iyueni.com/Uploads/group/icon/2/15475_zteE7W.jpg_200_200_2_70.jpg">>},
      {"groupname",<<231,190,164,231,187,132>>},
      {"groupid",<<"1122601">>}]}),

  ejabberd_router:route({jid,"1122601","super_group.yuejian.net",[],"1122601",
     "super_group.yuejian.net",[]}, {jid,"15538","yuejian.net",[],"15538","yuejian.net",[]}, {xmlel,<<"message">>,
            [{"id","1482128854632130"},
             {"isoff","1"},
             {"to","15538@yuejian.net"},
             {"msgtype","super_groupchat"},
             {"type","chat"},
             {"groupid","1122601"},
             {"mask","0"}],
            [{xmlel,<<"body">>,[],
                         [{xmlcdata,erlang:list_to_binary(CData)}]}]}).
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         


                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
%% 获取一天前的时间
timestamp(N1,N2) ->
 {M,S,SS} = now(),
 M*1000000000000+(S+(N1-N2)*3600)*1000000+SS .                      
                         
                         
                         
                         
                








                
                         
%% timestamp() ->
%%   {M,S,_} = erlang:now(),
%%   M*1000000 + S.
timestamp() ->
 {M,S,SS} = now(),
 M*1000000000000+S*1000000+SS.
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
                         
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
%% OfflineList = [{obj,[{"msgid",<<"1482130263232650">>},
%%               {"id",<<"1482130263232650">>},
%%               {"msgTime",<<"1482130263234">>},
%%               {"mask",<<"0">>},
%%               {"userid",<<"15475">>},
%%               {"username",<<"leon">>},
%%               {"userimage",
%%                <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200_200_2_80.jpg">>},
%%               {"usergender",1},
%%               {"type",1000},
%%               {"content",<<"1">>},
%%               {"msgsource",<<"XX">>},
%%               {"usersource",<<"yuejian.net">>},
%%               {"userfrom",<<"XX">>},
%%               {"msgtype",<<"0">>}]},
%%         {obj,[{"msgid",<<"1482130280891971">>},
%%               {"id",<<"1482130280891971">>},
%%               {"msgTime",<<"1482130280895">>},
%%               {"mask",<<"0">>},
%%               {"userid",<<"15475">>},
%%               {"username",<<"leon">>},
%%               {"userimage",
%%                <<"http://beta.iyueni.com/Uploads/avatar/2/15475_2VN7uq.jpg_200_200_2_80.jpg">>},
%%               {"usergender",1},
%%               {"type",1000},
%%               {"content",<<"2">>},
%%               {"msgsource",<<"XX">>},
%%               {"usersource",<<"yuejian.net">>},
%%               {"userfrom",<<"XX">>},
%%               {"msgtype",<<"0">>}]}],
%% {"mask":"0","offlinelist":[{"msgid":"1482128698706923","id":"1482128698706923","msgTime":"1482128698708","mask":"0","userid":"15209","username":"88882251","userimage":"http://beta.iyueni.com/Uploads/avatar/2/15209_O3j1yO.jpg_200_200_2_80.jpg","usergender":2,"type":1000,"content":"1","msgsource":"约你","usersource":"yuejian.net","userfrom":"约你","msgtype":"0"},{"msgid":"1482128700297505","id":"1482128700297505","msgTime":"1482128700299","mask":"0","userid":"15209","username":"88882251","userimage":"http://beta.iyueni.com/Uploads/avatar/2/15209_O3j1yO.jpg_200_200_2_80.jpg","usergender":2,"type":1000,"content":"2","msgsource":"约你","usersource":"yuejian.net","userfrom":"约你","msgtype":"0"},{"msgid":"1482128701884762","id":"1482128701884762","msgTime":"1482128701887","mask":"0","userid":"15209","username":"88882251","userimage":"http://beta.iyueni.com/Uploads/avatar/2/15209_O3j1yO.jpg_200_200_2_80.jpg","usergender":2,"type":1000,"content":"3","msgsource":"约你","usersource":"yuejian.net","userfrom":"约你","msgtype":"0"},{"msgid":"1482128757141686","id":"1482128757141686","msgTime":"1482128757143","mask":"0","userid":"15538","username":"Jim","userimage":"http://beta.iyueni.com/Uploads/avatar/2/15538_pgL9AK.jpg_200_200_2_80.jpg","usergender":1,"type":1000,"content":"4","msgsource":"约你","usersource":"yuejian.net","userfrom":"约你","msgtype":"0"},{"msgid":"1482128758676958","id":"1482128758676958","msgTime":"1482128758679","mask":"0","userid":"15538","username":"Jim","userimage":"http://beta.iyueni.com/Uploads/avatar/2/15538_pgL9AK.jpg_200_200_2_80.jpg","usergender":1,"type":1000,"content":"5","msgsource":"约你","usersource":"yuejian.net","userfrom":"约你","msgtype":"0"},{"msgid":"1482128789349801","id":"1482128789349801","msgTime":"1482128789351","mask":"0","userid":"15209","username":"88882251","userimage":"http://beta.iyueni.com/Uploads/avatar/2/15209_O3j1yO.jpg_200_200_2_80.jpg","usergender":2,"type":1000,"content":"6","msgsource":"约你","usersource":"yuejian.net","userfrom":"约你","msgtype":"0"}],"type":"10000","groupimage":"http://beta.iyueni.com/Uploads/group/icon/2/15475_zteE7W.jpg_200_200_2_70.jpg","groupname":"群组","groupid":"1122601"}
