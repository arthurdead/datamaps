#include <datamap.h>
#include <server_class.h>

#define private public
#include <core/HalfLife2.h>

extern IGameHelpers *gamehelpers;

void remove_datamap_from_sm_cache(datamap_t *pMap)
{
	CHalfLife2 *hl2 = (CHalfLife2 *)gamehelpers;
	hl2->m_Maps.removeIfExists(pMap);
}

void remove_serverclass_from_sm_cache(ServerClass *pMap)
{
	CHalfLife2 *hl2 = (CHalfLife2 *)gamehelpers;
	
	for(NameHashSet<DataTableInfo *>::iterator iter = hl2->m_Classes.iter(); !iter.empty(); iter.next()) {
		if((*iter)->sc == pMap) {
			delete *iter;
			iter.erase();
			break;
		}
	}
}
