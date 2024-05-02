using library 'lbpdeploy';

import 'std:object';
import 'std:gamemessagingmanager';
import 'std:string';

class HelloWorld extends Object
{
    pub fn OnCreate()
    {
        GameMessagingManager.Messaging_NewMessage(0, L'yo', L'man', L'why are there 3 of these', null);
    }
}