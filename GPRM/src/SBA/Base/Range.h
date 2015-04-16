/*
   A range is an object with attributes:
 */
#ifndef __RANGE__
#define __RANGE__
//#include <iostream>
namespace SBA {
    class Range {
        private:
            int start_;
            int stop_;
            int current_;
        public:
            inline bool next() {
                if (current_==stop_) {
                    return false;
                } else {
                    current_=current_+1;
                    //                std::cout << "Range: "<<start_<<" <= "<<current_ << " <= " << stop_ << "\n";
                    return true;
                }
            };
            inline int iter() {
                return current_;
            };
            inline unsigned int size() {
                return stop_ - start_;
            }
            inline int reset() {
                // std::cout << "Range: reset()\n";
                current_ = start_;
                return start_;
            }
            Range(int start, int stop) : start_(start), stop_(stop), current_(start) {};
    };
};
#endif // __RANGE__
