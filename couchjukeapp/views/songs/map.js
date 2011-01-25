function(doc) {
  if (doc.title) {
      emit(doc._id,{title: doc.title, album: doc.album, artist:doc.artist,cover: '/couchjuke/'+doc._id+'/cover', song : '/couchjuke/'+doc._id+'/song'});
  }
};