// Script provided by Aidan

using library 'lbpdeploy';

import 'std:triggercollectitem';
import 'std:poppet';
import 'std:gooey';
import 'std:resource';
from 'std:inventory' import *;
from 'std:lams' import Translate;

class TweakEgg(g27432) extends TriggerCollectItem
{
    private divergent ContentsIcon: Resource;
    private divergent CacheID: s32;

    pub fn StartTweakingObject(player: s32) -> bool
    {
        let cache_id = CreateInventoryCollection(player, 5, true);

        // User Created Objects

        AddView(player, cache_id, 128, 1610612736, L'POPIT_MY_PLANS');
        AddView(player, cache_id, 1024, 1610612736, L'POPIT_MY_PHOTOS');
        AddView(player, cache_id, 17825808, 0xE0000000, L'TG_My_Costumes');

        // Costumes Section

        AddView(player, cache_id, 16777248, 0, L'TG_Materials');
        AddView(player, cache_id, 16777232, GetCostumePartsHead(), L'TG_Head');
        AddView(player, cache_id, 16777232, GetCostumePartsBody(), L'TG_Body');

        // Stickers / Decorations

        AddView(player, cache_id, 8, 0, L'TG_Stickers');
        AddView(player, cache_id, 4, 0, L'TG_Decorations');

        // Tools

        AddView(player, cache_id, 65536, 0, L'POPIT_MATERIAL_TOOLS');
        AddView(player, cache_id, 32768, 0, L'POPIT_PAGE_GADGETS');
        AddView(player, cache_id, 512, 0, L'TG_Gameplay_Kits');
        AddView(player, cache_id, 4194304, 0, L'POPIT_PAGE_SOUND');
        AddView(player, cache_id, 256, 0, L'TG_Backgrounds');

        // Goodies

        AddView(player, cache_id, 1, 0, L'TG_Materials');
        AddView(player, cache_id, 2, 0, L'TG_Objects');

        this.CacheID = cache_id;

        return true;
    }

    pub fn StopTweakingObject()
    {
        DestroyInventoryCollection(this.CacheID);
    }

    pub fn UpdateUI(gooey: Gooey, poppet: Poppet)
    {
        if (!this.StartTweakFrame(gooey, 0x705FF58A)) return;
        Poppet.DoSectionBreak(gooey, Translate(0xD68620DD), true);

        if (gooey.StartFrame())
        {
            gooey.SetFrameSizing(-1.0f, 0.0f);
            gooey.SetFrameLayout(3, 3);
            gooey.SetSelectSound(L'poppet/menuselect');

            let input = gooey.DoImageButtonDottedBorder(
                10000,
                this.GetEggContentsIcon(),
                float2(192.0f, 192.0f),
                float2(128.0f, 128.0f),
                256
            );

            if (input & 256 != 0) 
                poppet.PushSubModeInventory(this.CacheID);
            else if (input & 4194304 != 0)
            {
                poppet.SetToolTip(
                    gooey,
                    Translate(0x1CBF49B8),
                    gooey.GetLastItemScreenRect(),
                    10000
                );
            }

            gooey.EndFrame();
        }

        Poppet.DoSectionBreak(gooey, Translate(0x110E4CFC), true);
        this.DoTweakWidget(gooey, poppet, this.GetThingUID(), 105);

        this.EndTweakFrame(gooey, poppet);
    }
}