SUBSCRIPTION
============

Test1 and Test2 become roster when they send an subscription each other.

* One direction Subscription

	  * Request
		
		  Test1 sends subscribe to Test2 to invite ::
			
			<presence from='test1@localhost/resource' to='test2@localhost' type='subscribe'/>

		  Server will forward this presence to Test2::
			
			<presence from='test1@localhost' to='test2@localhost/telnet' xml:lang='' type='subscribe'/>

		  And also response to Test1 to confirm ::
			
			<iq from='test1@localhost' to='test1@localhost/telnet' id='push3397374354' type='set'>
				<query xmlns='jabber:iq:roster'>
					<item ask='subscribe' subscription='none' jid='test2@localhost'/>
				</query>
			</iq>

	  * Accept

		  To accept invitation, Test2 will send an subscribed to accept::
			
			<presence from='test2@localhost/resource' to='test1@localhost' type='subscribed'/>

		  Server forward stanza to Test1 and also response to Test1 with an <iq> of subscription value 'to'::

		  	<presence from='test2@localhost' to='test1@localhost/telnet' type='subscribed'/>

			<iq from='test1@localhost' to='test1@localhost/telnet' id='push1338302721' type='set'>
				<query xmlns='jabber:iq:roster'>
					<item subscription='to' jid='test2@localhost'/>
				</query>
			</iq>

		  And the same time, server also responses to Test2 an <iq/> of subscrition 'from'::

			<iq from='test2@localhost' to='test2@localhost/telnet' id='push2422382305' type='set'>
				<query xmlns='jabber:iq:roster'>
					<item subscription='from' jid='test1@localhost'/>
				</query>
			</iq>

	  After this, now Test1 can see any status from Test2, but Test2 can not see Test1. 

* Bidirection Subscription

  	  * Request

	    To Test2 also sees status of Test1, Test2 sends to Test1 an subscribe ::
		
			<presence from='test2@localhost/resource' to='test1@localhost' type='subscribe'/>

	    Server forwards this stanza to Test1 and also response to Test1 an <iq> ::

		  	<presence from='test2@localhost' to='test1@localhost/telnet' type='subscribe'/>

			<iq from='test1@localhost' to='test1@localhost/telnet' id='push1297383213' type='set'>
				<query xmlns='jabber:iq:roster'>
					<item subscription='to' jid='test2@localhost'/>
				</query>
			</iq>
		
	    The same time, Test2 will receive an inform iq::

			<iq from='test2@localhost' to='test2@localhost/telnet' id='push105291863' type='set'>
				<query xmlns='jabber:iq:roster'>
					<item ask='subscribe' subscription='from' jid='test1@localhost'/>
				</query>
			</iq>

	  * Accept

		Test1 accepts invitation, he send an subscribed::
		    
		    <presence from='test1@localhost/resource' to='test2@localhost' type='subscribed'/>
		  
		Server will send to both Test1 and Test2 with subsciption 'both' ::

		  	<iq from='test2@localhost' to='test2@localhost/telnet' id='push3317716284' type='set'>
				<query xmlns='jabber:iq:roster'>
					<item subscription='both' jid='test1@localhost'/>
				</query>
			</iq>
		  
			<iq from='test1@localhost' to='test1@localhost/telnet' id='push1399557018' type='set'>
				<query xmlns='jabber:iq:roster'>
					<item subscription='both' jid='test2@localhost'/>
				</query>
			</iq>

.. Note::
 
   - subscription = 'both' : two users can see each other.
   

		
