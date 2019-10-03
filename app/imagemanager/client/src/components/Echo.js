// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React from 'react'
import { connect } from 'react-redux'
import { updateImage, getAllImages,
         deleteImage, getAllChapters, getAllLicences,
         openEditor, receiveNotification
} from '../actions'
import Images from './Images'
import EditImage from './EditImage'
import Notifications from './Notifications'

class Echo extends React.Component {
  constructor(props){
  	super(props);
    this.dispatch = props.dispatch;
  }

  handleMsg(evt) {
      let actions = {
          "update": updateImage,
          "all_images": getAllImages,
          "all_chapters": getAllChapters,
          "all_licences": getAllLicences,
          "create": undefined,
          "delete": deleteImage,
          "message": undefined,
          "notice": receiveNotification
      }

      let parsed = undefined;
      try {
          parsed = JSON.parse(evt.data);
      } catch(e) {
          console.log("error parsing JSON");
          console.log(evt.data)
          return;
      }

      const dispatcher = actions[parsed.type];
      if(dispatcher) {
          this.dispatch(dispatcher(parsed.details));
      } else {
          console.log("Unrecognised event")
          console.log(parsed)
      }
  }

  sendEvent(event_type, details) {
      let payload = {
          "event": event_type
      }
      if(details !== undefined) {
          payload["details"] = details
      }
      this.connection.send(JSON.stringify(payload))
  }

  deleteImage(hash) {
      this.sendEvent("delete_image", {
          "hash": hash
      })
  }

  editImage(hash, image) {
      this.dispatch(openEditor(hash, image))
  }

  saveImage(hash, image) {
      this.sendEvent("update_image", {
          "hash": hash,
          "image": image
      })
  }

  setRank(img, delta) {
      const newRank = parseInt(img.rank) + delta;
      this.sendEvent("set_rank", {
          "hash": img.id,
          "rank": newRank
      })
  }

  startUp() {
      this.sendEvent("get_all_chapters")
      this.sendEvent("get_all_images")
      this.sendEvent("get_all_licences")
  }

  handleOpen(e) {
      this.startUp()
  }

  handleError(error) {
      console.log(error)
  }

  handleClose(e) {
      this.connectWS()
  }

  connectWS() {
    const url = "ws://" + window.location.host + "/websocket";
  	this.connection = new WebSocket(url);
    this.connection.onmessage = evt => this.handleMsg(evt)
    this.connection.onerror = error => this.handleError(error)
    this.connection.onclose = evt => this.handleClose(evt)
    this.connection.onopen = evt => this.handleOpen(evt)
  }

  componentDidMount(){
    this.connectWS()
  }

  render() {
    return (
      <div>
        <Images images={this.props.images}
                deleteCb={(i) => this.deleteImage(i)}
                editCb={(id, image) => this.editImage(id, image)}
                setRankCb={(img, delta) => this.setRank(img, delta)}
        />
        <EditImage images={this.props.images}
                   licences={this.props.licences}
                   editor={this.props.editor}
                   saveCb={(id, image) => this.saveImage(id, image)}
        />
        <Notifications notifications={this.props.notifications} />
      </div>
    );
  }
};

export default connect()(Echo);
