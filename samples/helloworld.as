using library 'lbp2';

import 'std:thing';
import 'std:gamemessagingmanager';
import 'std:string';

class HelloWorld extends Thing
{
    pub fn OnCreate()
    {
        GameMessagingManager.Messaging_NewMessage(26, L'Top message', L'left button', L'right button', null);
    }
}