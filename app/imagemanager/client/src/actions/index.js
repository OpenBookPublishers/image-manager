// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
let nextImageId = 0
export const addImage = text => ({
  type: 'ADD_IMAGE',
  id: nextImageId++,
  text
})

export const updateImage = details => ({
  type: 'UPDATE_IMAGE',
  id: nextImageId++,
  details
})

export const getAllImages = details => ({
  type: 'GET_ALL_IMAGES',
  id: nextImageId++,
  details
})

export const deleteImage = id => ({
  type: 'DELETE_IMAGE',
  id
})

export const setVisibilityFilter = filter => ({
  type: 'SET_VISIBILITY_FILTER',
  filter
})

export const setChapter = id => ({
  type: 'SET_CHAPTER',
  id
})

export const getAllChapters = details => ({
  type: 'GET_ALL_CHAPTERS',
  id: nextImageId++,
  details
})

export const getAllLicences = details => ({
  type: 'GET_ALL_LICENCES',
  id: nextImageId++,
  details
})

export const openEditor = (id, image) => ({
  type: 'OPEN_EDITOR',
  id,
  image
})

export const closeEditor = () => ({
  type: 'CLOSE_EDITOR'
})

export const updateEditor = (key, value) => ({
  type: 'UPDATE_EDITOR',
  key,
  value
})

export const receiveNotification = (details) => ({
  type: 'RECEIVE_NOTIFICATION',
  details
})

export const closeNotifications = () => ({
  type: 'CLOSE_NOTIFICATIONS'
})

export const VisibilityFilters = {
  SHOW_ALL: 'SHOW_ALL',
  SHOW_COMPLETED: 'SHOW_COMPLETED',
  SHOW_ACTIVE: 'SHOW_ACTIVE',
  SHOW_CHAPTER: 'SHOW_CHAPTER'
}

