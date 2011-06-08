var ArtistList = Backbone.Collection.extend({
    url: '/artists',
    model: Artist,
    fetch: function(){
	db.view('couchjuke/artists?group=true',{success:function(res)
		{
		    _.each(res.rows, function(a){
			       artists.add({artist:a.key, album_count: a.value});
			   });
		}});
    }
}); 

var AlbumList = Backbone.Collection.extend({
    url : function(){
	return '/albums';					       
    },
    model : Album,
    fetch: function(artist){
	albums.refresh([]),
	db.view('couchjuke/albums_by_artist?startkey='+JSON.stringify(utf8_encode(artist))+'&endkey='+JSON.stringify(utf8_encode(artist)),{success:function(res)
	        {
		    _.each(res.rows,function(a){
			       a.value.cover = JSON.stringify(a.value.cover);
			       albums.add(a.value);
			   });
		}});
    }
 });

var SongList = Backbone.Collection.extend({
    url: function(){
        return '/songs';
    },
    model: Song,
    fetch: function(artist, album){
	songs.refresh([]);
	var key = '['+JSON.stringify(utf8_encode(artist))+', '+JSON.stringify(utf8_encode(album))+']';
	db.view('couchjuke/songs_by_album?startkey='+key+'&endkey='+key,{success:function(res)
		{
		    var Id = res.rows[0].id;
		    _.each(res.rows[0].value.songs,function(a){			       
			       songs.add({title:a.title, track_no: a.track_no, comment:a.comment, rating: a.rating, song_url: JSON.stringify('/couchjuke/'+Id+'/'+a.title), id: Id});
			   });
		}});
    }
});

var SongsToPlay = Backbone.Collection.extend({
    url: function(){
        return '/playlist/titles.json';
    },
    model: Song						
});

var Artist = Backbone.Model.extend({
    url: function(){
        return '/artists/'+this.get("_id");	  
    },
    id: '',
    artist: '',
    album_count: ''
});

var Album = Backbone.Model.extend({
    url : function(){
	return '/albums/'+this.get("id")+'.json';					       
    },
    id: '',
    artist: '',
    title: '',
    year: '',
    genre: 0,
    comment: '',
    cover: '', 
    track_count: 0
});


var Song = Backbone.Model.extend({
    url: function(){
        return '/songs/'+this.get("id")+'.json';
    },
    id: '',
    title: '',
    track_no: 0,
    comment: '',
    rating: 0,			     
    song_url: '',
    selected: false,
    add_to_playlist: function(){
        PlayList.add(this);
    },
    remove_from_playlist: function(){
        PlayList.remove(this);
    }
});

var ArtistView = Backbone.View.extend({
    model : Artist,
    tagName: "tr",
    template: _.template($("#artistTemplate").html()),
    albums: [],
    events: {
	"click .selectableArtist": "get_albums"
    },
    get_albums: function(k){
	if(this.model){
	    var key = this.model.get("artist");
	    
	    albums.fetch(key);
	};
    },

    initialize: function(){
	_.bindAll(this,'render');
    },
    render: function(){
	$(this.el).empty().html(this.template(this.model.toJSON()));
	return this;
    }
});

var ArtistListView = Backbone.View.extend({
    el: $("#artists tbody"),			  
	
    initialize : function(){
        _.bindAll(this,'render', 'addArtist');
	artists.bind('add', this.addArtist);
    },
    addArtist: function(artist){
        var view = new ArtistView({model: artist});
        var rendered = view.render().el;
        this.el.append(rendered);	
    }
});

var AlbumView = Backbone.View.extend({
    model : Album,
    tagName: "tr",
    template: _.template($("#albumTemplate").html()),
    events: {
	"click .selectableAlbum": "get_songs"
    },
    get_songs: function(k){
	if(this.model){
	    var album = this.model.get("title");
	    var artist = this.model.get("artist");
	    songs.fetch(artist,album);
	};
    },

    initialize: function(){
	_.bindAll(this,'render');
    },
    render: function(){
	$(this.el).empty().html(this.template(this.model.toJSON()));
	return this;
    }
});

var AlbumListView = Backbone.View.extend({
    el : $("#AlbumBlock"),
    template: _.template($("#albumsTemplate").html()),
    initialize : function(){
        _.bindAll(this, 'render', 'addAlbum');
	albums.bind('add', this.addAlbum);
	albums.bind('remove', this.render);
	albums.bind('refresh', this.render);
    },

    addAlbum: function(album){
        var view = new AlbumView({model: album});
        var rendered = view.render().el;
        this.el.append(rendered);	
    },
    render: function(){	
	$(this.el).empty().html(this.template);
	_.each(albums,function(a){
		   a.render();
	       });
	songs.refresh([]);
	return this;
    }
 });

var SongView = Backbone.View.extend({
    model: Song,
    tagName : "tr",
    template : _.template($("#songTemplate").html()),
    initialize: function(){
	_.bindAll(this,'render');
    },
    render: function(){
	$(this.el).empty().html(this.template(this.model.toJSON()));
	return this;
    }
});

var SongListView = Backbone.View.extend({
    el : $("#SongBlock"),
    template: _.template($("#songsTemplate").html()),
	
    initialize : function(){
        _.bindAll(this, 'render', 'addSong');
	songs.bind('add', this.addSong);
	songs.bind('remove', this.render);
	songs.bind('refresh', this.render);
    },

    addSong: function(song){
        var view = new SongView({model: song});
        var rendered = view.render().el;
        this.el.append(rendered);	
    },
    render: function(){
	$(this.el).empty().html(this.template);
	_.each(songs,function(s){
		   s.render();
	       });
	return this;	
    }
});

var CouchJukeController = Backbone.Controller.extend({
/*    routes: {
	"": home,
	"!/home":home,
	"!/artists/:artistId": loadArtist,
	"!/albums/:albumId": loadAlbum,
	"!/songs/:songId": loadSong
     },

     home: function(){
	 return this;
     },
     loadArtist: function(artistId){
	 return this;  
     },
     loadAlbum: function(albumId){
	 return this;
     },
     loadSong: function(songId){
	 return this;
     },
*/
     initialize: function(options){
	 this.PlayList = new SongsToPlay();
	 this.ArtistView = new ArtistListView({
					      model: artists
					  });
	 //TODO: load the artists from couchdb
         this.AlbumView = new AlbumListView({ model: albums});	 
         this.SongView = new SongListView({ model: songs});	 
	 artists.fetch();
	 this.ArtistView.render();
	 return this;
     }
});

$(function(){
      window.db = $.couch.db("couchjuke");
      window.artists = new ArtistList();
      window.albums = new AlbumList();
      window.songs = new SongList();
      var controller = new CouchJukeController();
      console.log("CouchJuke started.");
});

function utf8_encode (argString) {
    // Encodes an ISO-8859-1 string to UTF-8  
    // 
    // version: 1103.1210
    // discuss at: http://phpjs.org/functions/utf8_encode
    // +   original by: Webtoolkit.info (http://www.webtoolkit.info/)
    // +   improved by: Kevin van Zonneveld (http://kevin.vanzonneveld.net)
    // +   improved by: sowberry
    // +    tweaked by: Jack
    // +   bugfixed by: Onno Marsman
    // +   improved by: Yves Sucaet
    // +   bugfixed by: Onno Marsman
    // +   bugfixed by: Ulrich
    // *     example 1: utf8_encode('Kevin van Zonneveld');
    // *     returns 1: 'Kevin van Zonneveld'
    var string = (argString + ''); // .replace(/\r\n/g, "\n").replace(/\r/g, "\n");
    var utftext = "",
        start, end, stringl = 0;
 
    start = end = 0;
    stringl = string.length;
    for (var n = 0; n < stringl; n++) {
        var c1 = string.charCodeAt(n);
        var enc = null;
 
        if (c1 < 128) {
            end++;
        } else if (c1 > 127 && c1 < 2048) {
            enc = String.fromCharCode((c1 >> 6) | 192) + String.fromCharCode((c1 & 63) | 128);
        } else {
            enc = String.fromCharCode((c1 >> 12) | 224) + String.fromCharCode(((c1 >> 6) & 63) | 128) + String.fromCharCode((c1 & 63) | 128);
        }
        if (enc !== null) {
            if (end > start) {
                utftext += string.slice(start, end);
            }
            utftext += enc;
            start = end = n + 1;
        }
    }
 
    if (end > start) {
        utftext += string.slice(start, stringl);
    }
 
    return utftext;
}