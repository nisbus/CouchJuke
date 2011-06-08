%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(id3_v1).
-import(lists, [reverse/1]).
-export([read_id3_tag/1, to_int/1]).

read_id3_tag(File) ->
    case catch(file:open(File, [read,binary,raw])) of
        {ok, S} ->
	    Size = filelib:file_size(File),
	    %%Take care not to crash on invalid mp3 files
	    case Size < 128 of
		true ->
		    error;
		false ->
		    {ok, B2} = file:pread(S, Size-128, 128),
		    Result = parse_v1_tag(B2),
		    file:close(S),
		    case Result of
			{"ID3v1.1",_} ->
			    Result;
			{"ID3v1",_} ->
			    Result;
			_ ->
			    error
		    end
	    end;
        _Error ->
	    error_logger:error_msg("Error ~p~n",[_Error]),
            error
    end.

parse_v1_tag(<<$T,$A,$G,Title:30/binary, Artist:30/binary,Album:30/binary, Year:4/binary, Comment:28/binary, 0:8,Track:8, Genre:8>>) ->
    {"ID3v1.1", [{track,Track}, {title,trim(Title)},{artist,trim(Artist)}, {album, trim(Album)}, {year, to_int(Year)}, {comment, trim(Comment)}, {genre, trim(Genre)}]};

parse_v1_tag(<<$T,$A,$G,Title:30/binary, Artist:30/binary,Album:30/binary,  Year:4/binary, Comment:30/binary, Genre:8>>) ->
    {"ID3v1", [{title,trim(Title)},{artist,trim(Artist)}, {album, trim(Album)}, {year, to_int(Year)}, {comment, trim(Comment)}, {genre, trim(Genre)}]};

parse_v1_tag(_) ->
    error.

to_int(L) ->
    case catch (list_to_integer(L)) of
	{'EXIT',{badarg,_}} -> L;
	I ->
	    I
    end.


trim(Bin) when is_binary(Bin)-> 
    list_to_binary(trim_blanks(binary_to_list(Bin)));
trim(NonBin) when is_list(NonBin) ->
    NonBin;
trim(Other) ->
    Other.


trim_blanks(X) -> reverse(skip_blanks_and_zero(reverse(X))).

skip_blanks_and_zero([$\s|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T])   -> skip_blanks_and_zero(T);
skip_blanks_and_zero(X)       -> X.
