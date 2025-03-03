// Copyright Yahoo. Licensed under the terms of the Apache 2.0 license. See LICENSE in the project root.

#pragma once

#include "types.h"
#include "messages.h"
#include <vespa/storageapi/message/persistence.h>

namespace document { class BucketIdFactory; }

namespace storage {

namespace spi { struct PersistenceProvider; }
class PersistenceUtil;

/**
 * Handles most of the messages that are 'simple' to handle and do not
 * logically belong together with any particular group.
 * It is stateless and thread safe.
 */
class SimpleMessageHandler : public Types {
public:
    SimpleMessageHandler(const PersistenceUtil&,
                         spi::PersistenceProvider&,
                         const document::BucketIdFactory&);
    MessageTrackerUP handleGet(api::GetCommand& cmd, MessageTrackerUP tracker) const;
    MessageTrackerUP handleRevert(api::RevertCommand& cmd, MessageTrackerUP tracker) const;
    MessageTrackerUP handleCreateIterator(CreateIteratorCommand& cmd, MessageTrackerUP tracker) const;
    MessageTrackerUP handleGetIter(GetIterCommand& cmd, MessageTrackerUP tracker) const;
private:
    MessageTrackerUP handle_conditional_get(api::GetCommand& cmd, MessageTrackerUP tracker) const;

    const PersistenceUtil&           _env;
    spi::PersistenceProvider&        _spi;
    const document::BucketIdFactory& _bucket_id_factory;
};

} // storage

