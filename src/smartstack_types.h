//
// Created by zcobell on 2/23/23.
//

#ifndef SMARTSTACK_SMARTSTACK_TYPES_H
#define SMARTSTACK_SMARTSTACK_TYPES_H

#ifdef SMARTSTACK_USE_FLAT_MAP
#include "absl/container/flat_hash_map.h"
#else
#include <unordered_map>
#endif

namespace SmartStack::Types {

/* @brief HashMap
 *
 * This class is a wrapper around the options for a hashmap.  If the
 * SMARTSTACK_USE_FLAT_MAP macro is defined, the class will use the
 * absl::flat_hash_map class, otherwise it will use the std::unordered_map
 *
 * @tparam KeyType Type of the key
 * @tparam MappedType Type of the mapped value
 */
template <typename KeyType, typename MappedType>
struct HashMap {
#ifdef SMARTSTACK_USE_FLAT_MAP
  typedef absl::flat_hash_map<KeyType, MappedType> type;
#else
  typedef std::unordered_map<KeyType, MappedType> type;
#endif
};
}  // namespace SmartStack::Types

#endif  // SMARTSTACK_SMARTSTACK_TYPES_H
