#include <stdint.h>
#include <iostream>
#include <QByteArray>
#include <QDebug>

class hexdump {
	const uint8_t * const mData;
	const size_t mLen;

	char *formatLine(char line[80], size_t *offset) const;

	template<typename T, typename E>
	T& dumpTo(T& os, E e) const {
		char line[80];
		size_t offset = 0;
		while (!done(offset)) {
			os << formatLine(line, &offset);
			if (!done(offset)) os << e;
		}
		return os;
	}

public:
	hexdump(const uint8_t *data, size_t len)
		: mData(data), mLen(len)
	{
	}
	hexdump(const QByteArray& data)
		: mData(reinterpret_cast<const uint8_t*>(data.data()))
		, mLen(data.length())
	{}

	bool done(size_t offset) const { return (mLen - offset) == 0; }

	friend std::ostream& operator<<(std::ostream& os, const hexdump& dump) {
		return dump.dumpTo(os, std::endl<char, std::char_traits<char> >);
	}
	friend QDebug operator<<(QDebug os, const hexdump& dump) {
		return dump.dumpTo(os, endl);
	}
};
