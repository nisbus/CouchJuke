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
	 get_track_no/1,
	 get_track_name/1,
	get_timestamp/0]).

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
    filelib:fold_files(BaseDir, ".+\.mp3", true, fun(F, _AccIn) ->
							 couchjuke_queue:add(F,Db)
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
%%      ===Albums
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
mp3({mp3, File},Db) ->
    %% Tag = id3_v1:read_id3_tag(File),
    %% case Tag of 
    %% 	not_found ->
    R = create_music_record(File),
    %% 	    save({Db, R, File});
    %% 	_ ->
    %R = get_music_record(Tag),
    save({Db, R, File}),
%    end,
    couchjuke_queue:done(File).

%%======================================================================
%% @spec save({Db::couchDb(), Record::music_record, File::file()}) -> ignored
%%
%% @doc Saves the music record as a CouchDb document and then adds the file as an attachment.
%%      Also searches the files directory for a .jpg file and if it finds one, saves it as the cover attachment.
%% @end
%%======================================================================
save({Db, Record,File}) ->
    {ok, Fd} = file:read_file(File),
    {ok,Doc} = couchbeam:save_doc(Db, Record),
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
    end.

%%======================================================================
%% @doc if you prefer to have inline attachments (Base64 encoded) use this one.
%%======================================================================
save_inline({Db, Record,File}) ->
    {ok, Fd} = file:read_file(File),
    case get_cover(File) of
    	[] ->	    
    	    Doc = couchbeam_attachments:add_inline(Record, Fd, "song", "audio/mp3"),
    	    {ok,_Doc} = couchbeam:save_doc(Db, Doc);
    	[H|_T] ->		
	    DocCover = couchbeam_attachments:add_inline(Record, Fd, "song", "audio/mp3"),
	    {ok, Cover} = file:read_file(H),
	    Doc = couchbeam_attachments:add_inline(DocCover, Cover, "cover","image/jpg"),
	    couchbeam:save_doc(Db, Doc)
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
			AccIn++"/"++X
		end,"",T),
    filelib:fold_files(Dir,".+\.jpg",false, fun(F, AccIn) ->
						    [F|AccIn]
					    end,[]).

create_music_record(File) when is_binary(File) -> 
    WoExt = remove_file_extension(File),
    FileSplit = lists:reverse(re:split(binary_to_list(WoExt), "[/]")),
    [FileName|Rest] = FileSplit,
    [Album|Res] = Rest,
    [Artist|_Ignore] = Res,
    TrackNo = get_track_no(FileName),
    case TrackNo of
	not_found ->
	    {[generate_id(FileName, Artist, Album),{title, FileName},{album, Album},{artist,Artist},{track_no, 0},{timestamp,get_timestamp()}]};
	_ ->
	    TrackName = get_track_name(FileName),
	    {[generate_id(TrackName, Artist, Album),{title, TrackName},{album, Album},{artist,Artist},{track_no, drop_trailing_zeroes(TrackNo)}, {timestamp,get_timestamp()}]}
    end;

create_music_record(File) -> 
    create_music_record(list_to_binary(File)).

remove_file_extension(File) ->
    FileAsString = binary_to_list(File),
    case lists:dropwhile(fun(X) -> X /= $. end, lists:reverse(FileAsString)) of
	[] ->
	    <<"Invalid filename">>;
	[$.|T] ->
	    list_to_binary(lists:reverse(T));
	[_H|_T] ->
	    File
    end.

get_timestamp() ->
    {{Year,Month,Day},{Hour, Minute, Second}} =erlang:localtime_to_universaltime(erlang:localtime()),
   TS =  integer_to_list(Year)++"-"++string:right(integer_to_list(Month),2,$0)++"-"++string:right(integer_to_list(Day),2,$0)++" "++string:right(integer_to_list(Hour),2,$0)++":"++string:right(integer_to_list(Minute),2,$0)++":"++integer_to_list(Second),
    list_to_binary(TS).


get_track_no(<<"(",No:2/binary,") - ",_/binary>>) ->
    No;
get_track_no(<<No:2/binary," - ",_/binary>>) ->
    No;
get_track_no(_) ->
    not_found.

generate_id(Title, Artist, Album) ->
    {<<"_id">>,list_to_binary(binary_to_list(Artist)++" - "++binary_to_list(Album)++" - "++binary_to_list(Title))}.

get_track_name(<<"(",_No:2/binary,") - ",FileName/binary>>) ->
    FileName;
get_track_name(<<_No:2/binary," - ",FileName/binary>>) ->
    FileName;
get_track_name(FileName) ->
    FileName.

drop_trailing_zeroes(No) ->
    list_to_binary(lists:dropwhile(fun(X) -> X == $0 end, binary_to_list(No))).

%%=================================================================
%% TESTS
%% For some reason the tests don't pass as the hard coded expected 
%% results come out a bit weird.
%% If anyone knows what the deal is, it would be appreciated if it was fixed.
%%=================================================================
parse_binary_path_leading_track_no_test() ->
    TestPath = <<"/home/nisbus/Music/nisbus/lowercase/03 - lullabyte.mp3">>,
    Record = create_music_record(TestPath),
    ?assert(Record == {[<<"lullabyte - nisbus - lowercase">>,{title, <<"lullabyte">>},{album, <<"lowercase">>},{artist,<<"nisbus">>},{track_no, <<"3">>}]}).
    
parse_binary_path_leading_track_no_in_amperasands_test() ->
    TestPath = <<"/home/nisbus/Music/nisbus/lowercase/(03) - lullabyte.mp3">>,
    Record = create_music_record(TestPath),
    ?assert(Record == {[<<"lullabyte - nisbus - lowercase">>,{title, <<"lullabyte">>},{album, <<"lowercase">>},{artist,<<"nisbus">>},{track_no, <<"3">>}]}).

parse_binary_path_track_wo_track_no_test() ->
    TestPath = <<"/home/nisbus/Music/nisbus/lowercase/lullabyte.mp3">>,
    Record = create_music_record(TestPath),
    ?assert(Record == {[<<"lullabyte - nisbus - lowercase">>,{title, <<"lullabyte">>},{album, <<"lowercase">>},{artist,<<"nisbus">>},{track_no, <<"0">>}]}).

parse_string_path_leading_track_no_test() ->
    TestPath = "/home/nisbus/Music/nisbus/lowercase/03 - lullabyte.mp3",
    Record = create_music_record(TestPath),
    ?assert(Record == {[<<"lullabyte - nisbus - lowercase">>,{title, <<"lullabyte">>},{album, <<"lowercase">>},{artist,<<"nisbus">>},{track_no, <<"3">>}]}).
    
parse_string_path_leading_track_no_in_amperasands_test() ->
    TestPath = "/home/nisbus/Music/nisbus/lowercase/(03) - lullabyte.mp3",
    Record = create_music_record(TestPath),
    ?assert(Record =:= {[<<"lullabyte - nisbus - lowercase">>,{title, <<"lullabyte">>},{album, <<"lowercase">>},{artist,<<"nisbus">>},{track_no, <<"3">>}]}).

parse_string_path_track_wo_track_no_test() ->
    TestPath = "/home/nisbus/Music/nisbus/lowercase/lullabyte.mp3",
    Record = create_music_record(TestPath),
    ?assert(Record == {[<<"lullabyte - nisbus - lowercase">>,{title, <<"lullabyte">>},{album, <<"lowercase">>},{artist,<<"nisbus">>},{track_no, <<"0">>}]}).
