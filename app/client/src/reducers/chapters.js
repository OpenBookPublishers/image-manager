// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.

const defaults = () => {
  const chapter_names = [
      {
        label: "Unassigned chapters",
        value: "unassigned"
      },
      {
        label: "Cover image",
        value: "cover"
      },
      {
        label: "Chapter 1",
        value: "chapter1"
      },
      {
        label: "Chapter 3",
        value: "chapter2"
      },
      {
        label: "Chapter 3",
        value: "chapter3"
      },
      {
        label: "Chapter 4",
        value: "chapter4"
      },
      {
        label: "Chapter 5",
        value: "chapter5"
      }
    ];

  return {
    current: chapter_names[0].value,
    chapter_names: chapter_names
  }
}

const makeChapter = (detail) => {
    return {
        label: detail.chapter_name,
        value: detail.chapter_uuid,
        ordinal: detail.ordinal
    }
}

const chapters = (state = defaults(), action) => {
  switch(action.type) {
    case 'SET_CHAPTER':
      const new_chapter_name = action.id;
      const new_state = {
          current: new_chapter_name,
          chapter_names: state.chapter_names
      }
      return new_state
    case 'GET_ALL_CHAPTERS':
      if(action.details === "") {
        action.details = [] // sic
      }
      const chapters = action.details.map(detail => makeChapter(detail))
      return {
          current: chapters[0].value,
          chapter_names: chapters
      }
    default:
      return state
  }
}

export default chapters;
