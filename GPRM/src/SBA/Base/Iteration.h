/* The Iteration class takes a vector of Range objects
   and iterates through them in the order of occurence, to emulate nested loops.
 */
#ifndef __GPRM__ITERATION__
#define __GPRM_ITERATION__

#include <vector>
#include "Range.h"
//#include <iostream>
using namespace SBA;

namespace SBA {

    class Iteration {
        private:
            std::vector< Range* > ranges_;
            std::vector< int > iters_; // this is either the position in the vector or in the range
        public:

            inline bool next() {       
                unsigned int idx=ranges_.size()-1;
                bool done=true;
                std::vector< Range* >::const_reverse_iterator rit;
                for (rit= ranges_.rbegin();rit!=ranges_.rend();++rit) {
                    Range* r = *rit;
                    //        for (auto r : ranges_) {            
                    bool not_done = r->next();
                    //             std::cout <<"Iteration: next() "<<idx << ":"<<r->iter() << "\n";
                    done = !not_done;
                    if ( done ) {
                        //std::cout << "Iteration: next(): reset()\n ";
                        r->reset();
                        iters_[idx] = r->iter();
                    } else {
                        //                std::cout << "Iteration: next(): break\n ";
                        iters_[idx] = r->iter();

                        break;
                    }
                    idx--;
                } 
                // so if done is true for the last iteration, the whole iteration is finished
                return done;
                }; // if next returns false, the end of the iteration has been reached
                // iter(i) returns the ith iterator, base 0
                inline int iter(unsigned int idx) {
                    return iters_.at(idx);
                };
                inline bool done() {
                    return (next()==false);
                };
                Iteration( const std::vector< Range* >& ranges) : ranges_(ranges) {
                    std::vector< Range* >::const_iterator it;
                    for (it= ranges_.begin();it!=ranges_.end();++it) {
                        Range* r = *it;
                        iters_.push_back(r->iter());
                    }
                };

            };
    };
#endif // __GPRM_ITERATION__
