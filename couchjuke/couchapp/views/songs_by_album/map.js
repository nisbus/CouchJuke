function(doc) {
  if (doc.title) {
      emit([doc.artist, doc.title],{id: doc._id, songs: doc.songs});
  }
};
