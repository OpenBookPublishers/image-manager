// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.

const defaults = () => {
  return {
    isOpen: false,
    message: ""
  }
}

const notifications = (state = defaults(), action) => {
  switch(action.type) {
    case 'RECEIVE_NOTIFICATION':
      console.log(action)
      return {
        isOpen: true,
        message: action.details.message
      }
    case 'CLOSE_NOTIFICATIONS':
      return {
        isOpen: false,
        message: ""
      }
    default:
      return state
  }
}

export default notifications;
