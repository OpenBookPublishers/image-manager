// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import { connect } from 'react-redux'
import { deleteImage } from '../actions'
import ImageList from '../components/ImageList'
import { VisibilityFilters } from '../actions'

const getVisibleImages = (images, filter, current) => {
  switch (filter) {
    case VisibilityFilters.SHOW_ALL:
      return images
    case VisibilityFilters.SHOW_COMPLETED:
      return images.filter(t => t.completed)
    case VisibilityFilters.SHOW_ACTIVE:
      return images.filter(t => !t.completed)
    case VisibilityFilters.SHOW_CHAPTER:
      return images.filter(t => (t.chapter_uuid == current))
    default:
      throw new Error('Unknown filter: ' + filter)
  }
}

const mapStateToProps = state => ({
  images: getVisibleImages(state.images, state.visibilityFilter,
                          state.chapters.current),
  chapters: state.chapters,
  editor: state.editor,
  licences: state.licences,
  notifications: state.notifications
})

const mapDispatchToProps = dispatch => ({
  deleteImage: id => dispatch(deleteImage(id))
})

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(ImageList)
