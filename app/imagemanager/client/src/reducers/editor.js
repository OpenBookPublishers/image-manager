// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.

const empty_image = {
  id: undefined,
  text: undefined,
  resolution: undefined,
  format: undefined,
  res_category: undefined,
  chapter_uuid: undefined,
  completed: false
}

const defaults = () => {
  return {
    isOpen: false,
    hash: undefined,
    image: empty_image
  }
}

const editor = (state = defaults(), action) => {
  switch(action.type) {
    case 'OPEN_EDITOR':
      console.log(action)
      return {
        isOpen: true,
        hash: action.id,
        image: action.image
      }
    case 'CLOSE_EDITOR':
      return defaults()
    case 'UPDATE_EDITOR':
      return {
        ...state,
        image: {
          ...state.image,
          [action.key]: action.value
        }
      }
    default:
      return state
  }
}

export default editor;
