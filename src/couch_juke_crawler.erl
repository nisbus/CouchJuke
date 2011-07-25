%%%-------------------------------------------------------------------
%%% File    : couch_save.erl
%%% Author  : nisbus <nisbus@gmail.com>
%%% Description : A crawler that crawls a directory for music or movies 
%%% and saves the results to couchdb.
%%% 
%%% Created : 13 Jan 2011 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(couch_juke_crawler).
-export([music/1,music/2,movies/1,movies/2]).
-include_lib("kernel/include/file.hrl").
-include("../include/jsonerl.hrl").

%%====================================================================
%% Records
%%====================================================================
-record(album,
	{
	  id, title, artist, songs = [], cover, year, genre
	}).

-record(song,
	{
	  id, title, album, artist, track_no, file, comment, year, genre
	}).

-record(movie,
	{
	  file
	}).
%%====================================================================
%% API
%%====================================================================
%%@doc starts scanning the directory given and saves the movies to couchdb.
-spec movies(BaseDir:: string(),Db::string()) -> ok.
movies(BaseDir,Db) ->
    MovieList = get_movie_list(BaseDir),
    {ok,CDb} = couchc:open_db(Db),
    lists:foreach(fun(M) ->
			  CouchMovie = convert_movie(M),
			  save_movie(CDb,CouchMovie,M)
		  end,MovieList).

%% @doc same as calling movies(BaseDir,"couchjuke_movies") //default movie db
-spec movies(BaseDir:: string()) -> ok.		    
movies(BaseDir) ->
    movies(BaseDir,"couchjuke_movies").

%% @doc start scanning the given directory and saves albums found as documents in couchdb
-spec music(BaseDir::string(),Db::string()) -> ok.
music(BaseDir,Db) ->
    AlbumList = get_album_list(BaseDir),
     {ok,CDb} = couchc:open_db(Db),
    lists:foreach(fun(_S) ->
			  CouchAlbum = convert_album(_S),
			  save_album(CDb,CouchAlbum,_S)			  
		  end,AlbumList).

%% @doc same as calling music(BaseDir,"couchjuke_music") //default music db
-spec music(BaseDir::string()) -> ok.    
music(BaseDir) ->
    music(BaseDir,"couchjuke_music").

%%====================================================================
%% Helper functions
%%====================================================================
%% @hidden
utf(B) when is_binary(B) ->
    unicode:characters_to_binary(binary_to_list(B));
utf([L]) ->
    unicode:characters_to_binary(L);
utf(L) when is_list(L) ->
    unicode:characters_to_binary(L);
utf(O) ->
    O.
%%=======================================================================================
%%  Music related functions
%%=======================================================================================
get_album_list(BaseDir) ->
    error_logger:info_msg("Scanning direcory ~p~n",[BaseDir]),
    filelib:fold_files(BaseDir, ".+\.mp3", true, fun(F, AccIn) ->
							 Song = get_tag({mp3,F}),
							 case Song of
							     {error, _} ->
								 AccIn;
							     error ->
								 AccIn;
							     _ ->
								 case lists:keyfind(Song#song.album, #album.title, AccIn) of
								     false -> 
									 [#album{songs = [Song],
										 title = Song#song.album, artist = Song#song.artist, cover = get_cover(F), year = Song#song.year, genre = Song#song.genre}|AccIn];
								     Album ->
									 
									 lists:keyreplace(Song#song.album,#album.title,AccIn,Album#album{songs = [Song|Album#album.songs]})
								 end
							 end
						 end,[]).

get_tag({mp3, File}) ->
    Tag = id3_v1:read_id3_tag(File),
    case Tag of 
	error ->
	    error;
	_ ->
	    Song = get_music_record(Tag),
	    Song#song{file = File}
    end.

get_cover(File) ->
    Tokens = string:tokens(File,"/"),
    [_H|T] = lists:reverse(Tokens),
    Directory = lists:foldr(fun(X, AccIn) ->
			      case AccIn of 
				  "" ->
				      X;
				  _ ->
				      AccIn++"/"++X
			      end
		end,"",T),
    Dir = case hd(File) of
	      "/" -> 
		  "/"++Directory;
	      47 -> "/"++Directory;
	      _ ->
		Directory
	  end,
    Covers = filelib:fold_files(Dir,".+\.jpg",false, fun(F, AccIn) ->
							     [F|AccIn]
						     end,[]),
    case Covers of
	[] ->
	    undefined;
	[H|_T] ->
	    H
    end.  

convert_album(Album) ->
    Id = list_to_binary(binary_to_list(utf(Album#album.artist))++" - "++utf(binary_to_list(Album#album.title))),
    Songs = lists:map(fun(S) ->
			      {[{title, utf(S#song.title)}, {track_no, S#song.track_no}, {comment, utf(S#song.comment)}, {rating, 0}]}
		      end,Album#album.songs),
    {[{<<"_id">>, Id},{type, <<"music">>},{title, utf(Album#album.title)}, {artist, utf(Album#album.artist)}, {year, Album#album.year}, {genre,Album#album.genre}, {songs , Songs}, {comment, ""}, {rating, 0}]}.

save_album(Connection,CouchAlbum, Album) ->
    case catch couchc:save_doc(Connection, CouchAlbum) of
	{ok, Id,Rev} ->
	    {NewId,NewRev} = save_cover(Connection,Album, {Id,Rev}),
	    save_songs(Connection, Album, {NewId, NewRev});
	{error, Reason} ->
	    io:format("Error saving file ~p~n",[Reason]);
	conflict ->
	    io:format("Album already exists~n")	    
    end.

save_songs(Connection,Album, {Id, Rev}) ->
    lists:foldl(fun(X,AccIn) ->
			save_song(Connection,X,AccIn)			
		end,{Id, Rev},Album#album.songs).

save_song(Db,Song,{Id, Rev}) ->
    {ok, Fd} = file:read_file(Song#song.file),
    Length = filelib:file_size(Song#song.file),
    T = edoc_lib:escape_uri(binary_to_list(utf(Song#song.title))),
    case couchc:save_attachment(Db, Id, T, Fd, [{rev, Rev}, {content_type,"audio/mp3"}, {content_length, Length}]) of
	{ok, NewId, NewRev} -> {NewId,NewRev};
	{error, retry_later} ->
	    {Id,Rev};
	{error, Reason} ->
	    error_logger:error_msg("Error saving song ~p~n",[Reason]),
	    {Id,Rev};
	 Other ->
	    error_logger:info_msg("Save attachment returned ~p~n",[Other])
    end.

save_cover(Connection, Album, {Id,Rev}) ->
    case Album#album.cover of
	undefined ->
	    error_logger:info_msg("No cover found for album~n"),
	    {Id,Rev};
	Cover ->
	    {ok, Fd} = file:read_file(Cover),
	    Length = filelib:file_size(Cover),
	    {ok, NewId,NewRev} = couchc:save_attachment(Connection, Id, "cover", Fd, [{rev, Rev}, {content_type,"image/jpg"}, {content_length, Length}]),
	    {NewId,NewRev}
    end.    

get_music_record(error) ->
    error;
get_music_record({"ID3v1", [{title,Title},{artist,Artist}, {album, Album}, {year, Year}, {comment, Comment}, {genre, Genre}]}) ->
    Id  = generate_id(Title, Artist, Album),
    #song{id = Id, title=Title, album=Album, artist=Artist, track_no=0, year=Year, comment = Comment, genre = Genre};

get_music_record({"ID3v1.1", [{track,Track}, {title,Title},{artist,Artist}, {album, Album}, {year, Year}, {comment, Comment}, {genre, Genre}]}) ->
    Id  = generate_id(Title, Artist, Album),
    #song{id = Id, title=Title, album=Album, artist=Artist, track_no=Track, year=Year, comment=Comment, genre=Genre}.

generate_id(Title, Artist, Album) ->
    {<<"_id">>,list_to_binary(binary_to_list(Artist)++" - "++binary_to_list(Album)++" - "++binary_to_list(Title))}.


%%=======================================================================================
%%  Movie related functions
%%=======================================================================================
get_movie_list(BaseDir) ->
    error_logger:info_msg("Scanning direcory ~p~n",[BaseDir]),
    filelib:fold_files(BaseDir, ".+\.mpg", true, fun(F, AccIn) ->
							 AccIn++[F]
						 end,[]).

convert_movie(M) ->
    #movie{file = M}.

save_movie(_Db, _Record, _File) ->
    ok.
