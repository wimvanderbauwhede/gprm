#ifndef _LS_LET_
#define _LS_LET_

#include <unordered_map>
#include <deque>
#include "SBA/Types.h"
#include "SBA/Packet.h"
namespace SBA {
namespace Base {
        class Let {
            private:
                std::unordered_map<SBA::Symbol_t,SBA::Symbol_t> _store;
                std::unordered_map<SBA::Symbol_t,SBA::Subtask> _subtask;
                std::unordered_map<SBA::Symbol_t, std::deque<SBA::Subtask> > _pending_reads;
                std::unordered_map<SBA::Symbol_t, std::deque<SBA::Subtask> > _pending_streams;
            public:
                void assign(SBA::Symbol_t,SBA::Symbol_t);
                SBA::Subtask get_subtask(SBA::Symbol_t);
                void put_subtask(SBA::Symbol_t, SBA::Subtask);

                SBA::Symbol_t read(SBA::Symbol_t);
                void remove(SBA::Symbol_t);
                bool exists(SBA::Symbol_t);
                bool pending_reads(SBA::Symbol_t var);
                SBA::Subtask get_pending_read(SBA::Symbol_t var);
                void put_pending_read(SBA::Symbol_t var, SBA::Subtask read);
                bool pending_streams(SBA::Symbol_t var);
                SBA::Subtask get_pending_stream(SBA::Symbol_t var);
                void put_pending_stream(SBA::Symbol_t var, SBA::Subtask stream);
                void clear();
        };
}
}


#endif // _LS_LET_
