#include <datamap.h>

#define private public
#include <core/HalfLife2.h>

extern IGameHelpers *gamehelpers;

void remove_datamap_from_sm_cache(datamap_t *pMap)
{
	CHalfLife2 *hl2 = (CHalfLife2 *)gamehelpers;
	hl2->m_Maps.removeIfExists(pMap);
}
