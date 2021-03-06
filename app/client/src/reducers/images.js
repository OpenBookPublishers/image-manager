// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.

const makeImage = (detail) => {
    const img = {
        id: detail.hash,
        text: detail.image_name,
        resolution: detail.resolution,
        format: detail.format,
        res_category: detail.res_category,
        chapter_uuid: detail.chapter_uuid,
        rank: detail.rank,
        figure_id: detail.figure_id,
        licence_status: detail.licence_status,
        acceptability: detail.acceptability,
        completed: false
    };
    if(detail.optional) {
        console.log("optional details supplied");
        return {
            ...img,
            orig_artist: detail.optional.orig_artist,
            provenance: detail.optional.provenance,
            url: detail.optional.url,
            orig_year: detail.optional.orig_year,
            orig_medium: detail.optional.orig_medium,
            orig_title: detail.optional.orig_title,
            orig_size: detail.optional.orig_size,
            orig_title: detail.optional.orig_title
        }
    } else {
        return img;
    }
}

const compare = (a, b) => {
    if(a === b) {
        return 0
    } else if(a > b) {
        return 1
    } else {
        return -1
    }
}

const defaults = () => {
    return []
}

const images = (state = defaults(), action) => {
  switch (action.type) {
    case 'UPDATE_IMAGE':
      const hash = action.details.hash;
      const newImage = makeImage(action.details)

      const newState = () => {
        if(-1 === state.findIndex((item) => item.id === hash)) {
          return [
            newImage,
            ...state
          ]
        }
        return state.map((i) => (i.id === hash) ? newImage : i)
      }
      return newState().sort((a, b) => compare(a.rank, b.rank))
    case 'GET_ALL_IMAGES':
      if(action.details === "") {
          action.details = [] // sic
      }
      const images = action.details.map(detail => makeImage(detail))
      return images
    case 'DELETE_IMAGE':
      return state.filter(item => item.id !== action.id.hash)
    default:
      return state
  }
}

export default images
