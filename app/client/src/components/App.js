// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React from 'react'
import TopAppBar, {
  TopAppBarFixedAdjust,
  TopAppBarIcon,
  TopAppBarRow,
  TopAppBarSection,
  TopAppBarTitle,
} from '@material/react-top-app-bar';
import MaterialIcon from '@material/react-material-icon';

import VisibleImageList from '../containers/VisibleImageList'

import '@material/react-top-app-bar/dist/top-app-bar.css';
import '@material/react-material-icon/dist/material-icon.css';

const App = () => (
  <div>
      <TopAppBar>
        <TopAppBarRow>

          <TopAppBarSection align='start'>
            <TopAppBarTitle>OBP Image Manager</TopAppBarTitle>
          </TopAppBarSection>

          <TopAppBarSection align='start'>
          </TopAppBarSection>

          <TopAppBarSection align='end' role='toolbar'>
            <TopAppBarIcon actionItem tabIndex={0}>
              <a href="/public/using-image-manager.pdf"><MaterialIcon
                aria-label="print page"
                hasRipple
                icon='help'
              /></a>
            </TopAppBarIcon>
            <TopAppBarIcon actionItem tabIndex={1}>
              <MaterialIcon
                aria-label="print page"
                hasRipple
                icon='bug_report'
                onClick={() => console.log('print')}
              />
            </TopAppBarIcon>
            <TopAppBarIcon actionItem tabIndex={2}>
              <MaterialIcon
                aria-label="print page"
                hasRipple
                icon='eject'
                onClick={() => console.log('print')}
              />
            </TopAppBarIcon>
          </TopAppBarSection>
        </TopAppBarRow>
      </TopAppBar>

      <TopAppBarFixedAdjust>
        <VisibleImageList />
      </TopAppBarFixedAdjust>
  </div>
)

export default App
