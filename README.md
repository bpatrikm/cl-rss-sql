# cl-rss-sql

A utility to track and view (some) RSS published content

Depends on SBCL, clsql, xmls, cl-ppcre and cl-gtk-frank. With clsql, only mysql has been tested as a backend. I am unsure if the parsing of dates will work with other backends, and the ':database-type' would need to be edited in a few places. The only tested OS is Linux. For downloading RSS files and playing content 'wget', 'youtube-dl' and 'mpv' need to be available.

A database needs to be available for clsql to use. The credentials should be placed in cl-rss-sql-cred.lisp. The table structure is available in 'tables.mysql', which is the output of mysqldump (and should be importable into a running server). The mysql server is expected to be using utf-8 for client communication.

There are two components in this utility.

## Tracker

The executable produced by 'sbcl --load "\<path\>/compile.lisp"' is intended to be run as a cronjob.

A directory called '.cl-rss-sql' needs to be created in the home directory of the user running the cronjob. RSS files are temporarily downloaded to this directory.

For each RSS source to follow, a line needs to be created in the database table channel. The column name is what will appear in the client as the channel name. Typically, this should be a unique string, but that isn't strictly needed. In order to treat several RSS sources as belonging to the same channel, the corresponding lines can be given the same name here. The column site can be one of youtube, bitchute and podcasts. The column site_channel_id is used if the site is youtube or bitchute, and should be the channel-specific part of the URL to the channel page. For youtube, this should be the string of characters starting with 'UC' and not the channel name. If the site is podcasts, the column rss_url is used instead of site_channel_id, and should hold a complete URL to an RSS file. The column site_channel_id still needs to hold a unique string, however. Every time the cronjob runs, the time elapsed since the last download of the RSS file is checked against the value in minutes_refresh_rate. The file is only downloaded again if more time has elapsed.

## Gui client

Require ':cl-rss-sql-client', and run 'cl-rss-sql-client:start-browse-rss'. Double-clicking a line will play the content. The refresh button will become active whenever newly published, undisplayed content is available in the database. Clicking it will then update the list in the client.

The remaining buttons will condense all rows having the same channel name is the currently selected row, into a single expandable row, and vice versa. Such a collapsed set of rows will be given a containting row, which will act as a proxy for the most recently published unwatched row, meaning that double-clicks and 'Mark as viewed'-clicks will apply to the proxied row instead.

## Coded filters

The above condensation can also be made permanent by setting \*channel-filters\* to a list of browse-filter-channels. If so desired, this should be done in the optional file 'cl-rss-sql-client-filters.lisp', containing something like:
    (in-package :cl-rss-sql-client)

    (defparameter *channel-filters*
      (list
       (make-instance 'browse-filter-channel
                      :ident (intern "<channel name>")
                      :filter-function (lambda (bis)
                                         (channel-collapse-filter bis "<channel name>")))
	...
	))

A more involved form of condensation can only be achieved by setting \*other-filters\* (in the same file):

    (defparameter *other-filters*
      (list
       (make-instance 'browse-filter
                      :filter-function
                      (make-grand-collapse-filter
                       "Display name"
                       (list (list "<channel name>"
                                   (make-title-collapse-filter "\\.*A series of videos, episode (\\d*)" t)
                                   <more title-collapse-filters>
	    		       )
                         
                             <more channel names>
		    	 )))
       <more browse-filters>
       ))

There are two levels of grouping here. First a set of channels are condensed together, into a topic, say. The title-collapse-filters will then group up all the content from that channel, whose name matches the given regular expression. It is optional to extract an episode number, as is done in the example, but doing so will cause the matching rows to be sorted according to that number, rather than the publishing date. The containing row of a title-collapse-filter also proxies a contained row, in this case the oldest unwatched row. Any content not matching a title-collapse-filter is grouped into 'Others'-><channel name>.

The design choices of these filter functions are (clearly) based on my own personal preferences. Future versions may offer additional customisation.