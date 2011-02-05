%%%-------------------------------------------------------------------
%%% File    : couch_save.erl
%%% @author  : nisbus
%%% @version : 0.0.1
%%% Description : Filesystem crawler for finding MP3s and their cover 
%%% files. All files found will be uploaded to CouchDB as attachments.
%%%
%%% Created : 13 Jan 2011 by nisbus
%%%-------------------------------------------------------------------
-module(couch_juke_crawler).
-export([start/5,
	 mp3/2,
	 get_cover/1,
	 create_music_record/1, 
	 get_track_name/1,
	 get_timestamp/0,
	 covers_only/4]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% @spec start(BaseDir::string(), Concurrent::int(), DatabaseName::string(), ServerUrl::string(), Port::int()) -> List
%% @doc Starts the crawler. 
%%   Starts from the basedir and includes subdirectories.
%%   Concurrent: indicates how many processes you want to have concurrently
%%   running for saving files, this depends a lot on the available memory. 
%%   DatabaseName: is the name of the database to create documents in.
%%   ServerUrl: is the url of the CouchDb instance.
%%   Port: is the port of the CouchDb instance.
%% @end
%%====================================================================
start(BaseDir,Concurrent, DatabaseName, ServerUrl, Port) ->
    couchbeam:start(),
    {ok,Db} = connect(DatabaseName, ServerUrl, Port),
    couchjuke_queue:start_link(Concurrent),
    {ok, View} = couchbeam:all_docs(Db),
    io:format("Getting existing files~n",[]),
    Existing = couchbeam_view:fold(View,fun(D,AccIn) ->
    						{[{_,Id},{_,_},{_,_}]} = D,
    						[Id|AccIn]
    					end),
    io:format("Existing files = ~p, filtering out existing from the new list ~n",[count(Existing)]),
    Files = filelib:fold_files(BaseDir, ".+\.mp3", true, fun(F, AccIn) ->
								 Record = create_music_record(F),
								 {[{_,Id},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_}]} = Record,
								 case lists:member(Id, Existing) of
								     true -> AccIn;
								     false ->
									 [{F,Record}|AccIn]
								 end
    							 end,[]),
    io:format("Found ~p new files~n",[count(Files)]),
    lists:foreach(fun(F) ->		
			  {File, Record} = F,
    			  couchjuke_queue:add(File, Record, Db)
    		  end,Files).

covers_only(BaseDir, DatabaseName, ServerUrl, Port) ->
    couchbeam:start(),
    {ok,Db} = connect(DatabaseName, ServerUrl, Port),
    filelib:fold_files(BaseDir, ".+\.mp3", true, fun(F, AccIn) ->
							 Record = create_music_record(F),
							 {[{_,Id},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_},{_,_}]} = Record,
							 case get_cover(F) of
							     [] ->
								 void;
							     [H|_T] ->
								 case couchbeam:open_doc(Db,Id) of
								     {ok, Doc} ->
									 {DocId, Rev} = couchbeam_doc:get_idrev(Doc),
									 {ok, Cover} = file:read_file(H),
									 CoverLength = filelib:file_size(H),
									 _CoverAtt = couchbeam:put_attachment(Db, DocId, "cover", Cover, [{rev, Rev},{content_type, "image/jpg"}, {content_length, CoverLength}]);
									     _ ->
									 void
								 end
							 end,
							 [{F,Record}|AccIn]
						 end,[]).
    
%%====================================================================
%% @spec connect(Db::string(), Server::string(), Port::int()) -> List
%% @doc Connects to the couchdb instance
%% @end
%%====================================================================
connect(Database, Server, Port) ->
    Connection = couchbeam:server_connection(Server, Port, "", []),
    couchbeam:open_db(Connection, Database, []).

%%=====================================================================
%% @spec mp3({mp3, File::string()}, Db) -> void
%%
%% @doc
%% This code originally contained code from the Pragmatic programmers bookshelf 
%% "Programming Erlang" which I highly reccommend
%% I've left it with the code as it was (commented out) for those of you
%% who want to use it but as it is copyrighted I can not include it in
%% this public repository.
%% 
%% Currently it parses the file path of each file to the following:
%% Artist, Album, TrackNo, Track 
%% This is how I organize my music library on disk i.e.
%%    Artist
%%      |
%%      ===Year - Albums
%%            |
%%            ==== TrackNo - Track
%%
%%   The path being parsed would then look like:
%%          nisbus/lowercase/04 - lullabyte.mp3
%%   resulting in:
%%     {["nisbus - lowercase - lullabyte", {artist: "nisbus"}, {album: "lowercase"},{track_no: 4}, {title: "lullabyte"}]}
%%
%%   note that the first item is the CouchDb Id of the file which is to get rid of duplicates.
%% @end
%%=====================================================================
mp3({mp3, File,Record},Db) ->
    %% Tag = id3_v1:read_id3_tag(File),
    %% case Tag of 
    %% 	not_found ->
    %R = create_music_record(File),
    %% 	    save({Db, R, File});
    %% 	_ ->
    %R = get_music_record(Tag),
    save_music({Db, Record, File}),
%    end,
    couchjuke_queue:done(File,Record).

%%======================================================================
%% @spec save({Db::couchDb(), Record::music_record, File::file()}) -> ignored
%%
%% @doc Saves the music record as a CouchDb document and then adds the file as an attachment.
%%      Also searches the files directory for a .jpg file and if it finds one, saves it as the cover attachment.
%% @end
%%======================================================================
save_music({Db, Record,File}) ->
    {ok, Fd} = file:read_file(File),
    case couchbeam:save_doc(Db, Record) of
	{ok,Doc} ->
	    Id = couchbeam_doc:get_id(Doc),
	    Rev = couchbeam_doc:get_rev(Doc),
	    Length = filelib:file_size(File),
	    {ok, {NewDoc}} = couchbeam:put_attachment(Db, Id, "song", Fd, [{rev, Rev},{content_type, "audio/mp3"}, {content_length, Length}]),
	    {<<"rev">>, NewRev} = hd(lists:reverse(NewDoc)), 
	    case get_cover(File) of
		[] ->
		    void;
		[H|_T] ->
		    {ok, Cover} = file:read_file(H),
		    CoverLength = filelib:file_size(H),
		    _CoverAtt = couchbeam:put_attachment(Db, Id, "cover", Cover, [{rev, NewRev},{content_type, "image/jpg"}, {content_length, CoverLength}])
	    end;
	{error,conflict} ->
	    io:format("File already exists ~p~n",[File]),
	    ok;
	{error, Reason} ->
	    io:format("Error saving file ~p, ~p~n",[File,Reason]),
	    stop
    end.


%%=======================================================================
%% @spec get_cover(File::string()) -> List
%% @doc
%% @end
%%=======================================================================
get_cover(File) ->
    Tokens = string:tokens(File,"/"),
    [_H|T] = lists:reverse(Tokens),
    Dir = lists:foldr(fun(X, AccIn) ->
			      case AccIn of
				  "" ->
				      X;
				  _ ->
				      AccIn++"/"++X
			      end
		end,"",T),
    filelib:fold_files(Dir,".+\.jpg",false, fun(F, AccIn) ->
						    [F|AccIn]
					    end,[]).

create_music_record(File) when is_list(File) -> 
    WoExt = remove_file_extension(File),
%    io:format("splitting ~s~n",[WoExt]),
    FileSplit = lists:reverse(re:split(WoExt, "[/]",[{return,list}])),
    [FileName|Rest] = FileSplit,
    [Album|Res] = Rest,
    [Artist|_Ignore] = Res,    
    {TrackNo,TrackName} = get_track_name(unicode:characters_to_binary(FileName,latin1,utf8)),
    UAlbum = unicode:characters_to_binary(Album,latin1),
    UArtist = unicode:characters_to_binary(Artist,latin1),
    {Year,Alb} = get_year_from_album(UAlbum),
    {[generate_id(TrackName, UArtist, Alb),{type, music},{title, TrackName},{album, Alb},{artist,UArtist},{track_no, drop_trailing_zeroes(TrackNo)},{year, Year},{timestamp,get_timestamp()}]};

create_music_record(File) when is_binary(File)-> 
    create_music_record(binary_to_list(File)).

remove_file_extension(File) ->
    case lists:dropwhile(fun(X) -> X /= $. end, lists:reverse(File)) of
	[] ->
	    <<"Invalid filename">>;
	[$.|T] ->
	    lists:reverse(T);
	[_H|_T] ->
	    File
    end.

get_timestamp() ->
    {{Year,Month,Day},{Hour, Minute, Second}} =erlang:localtime_to_universaltime(erlang:localtime()),
   TS =  integer_to_list(Year)++"-"++string:right(integer_to_list(Month),2,$0)++"-"++string:right(integer_to_list(Day),2,$0)++" "++string:right(integer_to_list(Hour),2,$0)++":"++string:right(integer_to_list(Minute),2,$0)++":"++integer_to_list(Second),
    list_to_binary(TS).


generate_id(Title, Artist, Album) ->
    {<<"_id">>,list_to_binary(binary_to_list(Artist)++" - "++binary_to_list(Album)++" - "++binary_to_list(Title))}.

get_track_name(<<"(",TrackNo:2/binary,") - ",FileName/binary>>) ->
    {TrackNo, FileName};
get_track_name(<<TrackNo:2/binary," - ",FileName/binary>>) ->
    {TrackNo, FileName};
get_track_name(<<TrackNo:2/binary,". ",FileName/binary>>) ->
    {TrackNo, FileName};
get_track_name(<<TrackNo:2/binary," ",FileName/binary>>) ->
    {TrackNo, FileName};
get_track_name(<<"[",TrackNo:2/binary,"] - ",FileName/binary>>) ->
    {TrackNo, FileName};
get_track_name(<<TrackNo:2/binary,"-",FileName/binary>>) ->
    {TrackNo, FileName};
get_track_name(FileName) ->
    {0, FileName}.

drop_trailing_zeroes(0) ->
    0;
drop_trailing_zeroes(No) ->
    list_to_binary(lists:dropwhile(fun(X) -> X == $0 end, binary_to_list(No))).

get_year_from_album(<<"(",Year:4/binary,") - ",Album/binary>>) ->
    {Year,Album};
get_year_from_album(<<"(",Year:4/binary,") ",Album/binary>>) ->
    {Year,Album};
get_year_from_album(<<Year:4/binary," - ",Album/binary>>) ->
    {Year,Album};
get_year_from_album(<<Year:4/binary," ",Album/binary>>) ->
    {Year,Album};
get_year_from_album(Album) ->
    {<<"unknown">>,Album}.

count([]) -> 0;
count([_|T]) -> 1 + count(T).

parse_test() ->
   F = "D:/shares/Music/1 12/2000 - no title/01 - intro.mp3",
   create_music_record(F). 
