// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React from 'react'
import { connect } from 'react-redux'
import Button from '@material/react-button';

import '@material/react-button/dist/button.css';

const imageFieldName = "thefile"; // must match server
const imageChapterUuid = "X-Chapter-Uuid"; // must match server

const handleUpload = (file, chapterUuid) => {
    const url = "http://" + window.location.host + "/api/upload";
    const req = new XMLHttpRequest();
    req.open("POST", url, true);

    const formData = new FormData();
    formData.append(imageFieldName, file);
    req.setRequestHeader(imageChapterUuid, chapterUuid);
    req.send(formData);

    return req
};

const doUpload = (evt, getChapterUuid) => {
    const chapterUuid = getChapterUuid();
    for(var f of evt.target.files) {
        handleUpload(f, chapterUuid);
    }
};

const Uploader = ({ onClick, completed, text, chapters }) => {
    const getChapterUuid = () => {
        return chapters.current;
    }

    return (
         <Button raised>
           <input type="file" multiple
                  onChange={(evt) => doUpload(evt, getChapterUuid)} />
         </Button>
    );
}

export default connect()(Uploader);
