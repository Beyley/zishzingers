using library 'lbp2';

import 'std:thing';
import 'std:gamemessagingmanager';
import 'std:string';

class HelloWorld extends Thing
{
    pub fn OnCreate()
    {
        GameMessagingManager.Messaging_NewMessage(0, L'yo', L'man', L'why are there 3 of these', null);
    }
}