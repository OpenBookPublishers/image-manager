OBP Image Manager usage
=======================

Author: Martin Keegan

Date: 2020-01-22

Last updated: 2020-01-22

Distribution: public

Basic usage
===========

Whether you are an editor or the author of the book, the process is much
the same; there is a specific "instance" of the image manager for each book.

Accessing the image manager in a web browser should bring up a page with
two bars across the top. The second, grey, bar is crucial. Use it to focus
on a particular chapter of the book.

When focused on the desired chapter, click "Choose Files" and select some
image files from your own computer to upload. These should gradually appear
in your web-browser, along with some metadata. For each image, you can do
the following five things, represented by icons to the left of the
metadata

* view the image - click on the thumbnail
* delete the image - click on the (-) button
* move the image to a position earlier in the chapter - click on the up button
* move the image to a position earlier in the chapter - click on the down button
* edit the image metadata - click on the pen icon

BUG: There is a known problem with chapters: the system periodically resets
to focus on the first chapter.

BUG: Sometimes the edit icon does not work. This can be obviated by
reloading the webpage, either with the icon in the browser or,
customarily, by pressing Ctrl-R, and then clicking the icon again.


Initialisation
--------------

To create a new instance of the image manager for a new book we need:

* the name of the principal author, editor or contributor
* the number of chapters

In the current version of the image manager, it takes five or ten minutes
to set up a new instance.

Background
==========

We are creating a small web-based tool for saving effort involved in
managing the collection of images to be used in books. It is likely that
there is some minimum threshold, e.g., 20 images, beyond which such a tool
could reduce the effort expended. It is also intended to make things easier
for editors such that some of the effort can be offloded onto the author
him/herself.

A tool like this was identified by production staff as the preferred
priority for workflow automation at the off-site day-long meeting held
with Adam Hyde of Coko / Editoria / Book Sprints in 2019Q1.

Image formats
-------------

The only image formats to be supported shall be those in the Author's Guide:

* JPEG
* TIFF
* PNG
* PDF

The system shall reject attempts to upload material that cannot be
automatically determined to be in these formats.

Resolution
----------

Each image shall be automatically determined to belong to one and only one
of the following categories, lifted directly from the Authors' Guide:

* Full-page images, including the cover image, should be approx. 3200 x 2500
pixels.
* Half-page images, which should be at least 2100 x 1700 pixels.
* Smaller images, which should be at least 800 x 500 pixels.
* Images that are to be rejected for being too small

Copyright status
----------------

The system shall enforce the requirement that every image be accompanied by
one and only one copyright status, which may be updated by the user and must
be from the following list:

* To Be Determined
* Public Domain
* All Rights Reserved
* CC-BY
* CC-BY-SA
* Fair use

This list presumably is incomplete and needs a few more isotopes of
the Creative Commons licence.
Image status
------------

Each image shall have an automatically-determined status, which shall
be available to the end-user in some fashion. The status shall be one
and only one of the following:

* accepted
* resolution too low
* licence required
* no appropriate caption

This list may be expanded in the light of hindsight, with a view to
getting the author to do more of the work and enabling editors to
survey progress efficiently.

Out of scope
------------

The following can be added later, and won't be considered for the present
effort:

* Usernames / passwords / authentication / user roles
* Managing multiple books
* Anything other than images, e.g., audio or video files
* Image sets which share a sequence number but which have one caption per image
* Export of caption information
* Uploading images without associating them with a chapter
* Editing chapter names / changing the number of chapters
* Recording / editing the optional image metadata
* Tracking of number of in-flight image uploads
