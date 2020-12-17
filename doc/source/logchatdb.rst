DATABASE
========

There are two databases are used to store log chat those are mongodb and postgresql. To respective to our system, mongodb will be used to contain just the last limited number of messages - default is 50 of each communication. Postgresql will used to contain all log chat. Follow is database design for our log chat.

C2C
---

1. Mongodb
   
   .. code-block:: javascript

		{
			"JidFrom": "String", 
			"JidTo": "String", 
			"Body": 
				[
					{
						"uid" : "integer",
						"Jid": "String (Jid of sender)", 
						"message": "String", 
						"file": "String (thumbnailUrl;mimeType;fileUrl;size;name)", 
						"time": "Time"
					}
				]
		}

 * JidFrom and JidTo is pair bareJIDs of sender and receiver (don’t care about the order)
 * Body
		* Each message will be assigned by an unique uid.
		* Jid is bareJid of sender
		* message is content of message body
		* file is content of attached file, File is a string that content all info of file and separate by “;”
		* time is time of sended message

2. Postgresql

   There are two tables utilized those are HistoryChat and HistoryChatMessage.

   HistoryChat Table:

	   	==================   ============   ============
		JidFrom              JidTo          bodyId
		==================   ============   ============
		==================   ============   ============

   HistoryChatMessage Table:

	   	========  ======  ==========  =======  =======   =======
	   	bodyId    uid     message     file     event     stamp
	   	========  ======  ==========  =======  =======   =======
	   	========  ======  ==========  =======  =======   =======

   .. Note::

   	  - ChatHistory has many Messages.
   	  - event column is only used for muc.

MUC
---

1. Mongodb

   .. code-block:: javascript

   		{
		    "Jid": "String", 
		    "JidMember": "String (jid1;jid2;jid3;...)", 
		    "Body": 
		    	[
			        {
			            "uid": "integer",
			            "Jid": "String (Jid of sender)", 
			            "message": "String", 
			            "file": "String (thumbnailUrl;mimeType;fileUrl;size;name)",
			            "event": "String ("joined" or "left"),
			            "time": "Time"
			        }
		    	]
		}
  
  * Jid is bareJID of room.
  * JidMember: is list of member’s jid, separete by “;”
  * Body
		* Jid is bareJid of sender
		* message is content of message body
		* file is content of attached file, File is a string that content all info of file and separate by “;”
		* event to save status joined or left of user in room.
		* time is time of sended message

2. Postgresql

   HistoryChat Table:

	   	==================   ============   ============
		JidFrom              JidTo          bodyId
		==================   ============   ============
		==================   ============   ============

   HistoryChatMessage Table:

	   	========  ======  ==========  =======  =======   =======
	   	bodyId    uid     message     file     event     stamp
	   	========  ======  ==========  =======  =======   =======
	   	========  ======  ==========  =======  =======   =======

   .. Note::

   	  ChatHistory has many Messages.





 