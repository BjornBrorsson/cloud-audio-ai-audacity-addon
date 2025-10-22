"""Voice Library browser for discovering and adding ElevenLabs voices."""

import sys
from pathlib import Path
from typing import Optional, Dict, Any, List

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from utils import Config, ElevenLabsAPI


class VoiceLibraryBrowser:
    """Browse and discover voices from the ElevenLabs Voice Library."""
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the voice library browser.
        
        Args:
            api_key: ElevenLabs API key (optional)
        """
        Config.validate()
        self.api = ElevenLabsAPI(api_key)
    
    def search_voices(
        self,
        query: Optional[str] = None,
        gender: Optional[str] = None,
        age: Optional[str] = None,
        accent: Optional[str] = None,
        language: Optional[str] = None,
        use_cases: Optional[List[str]] = None,
        page_size: int = 30
    ) -> Dict[str, Any]:
        """
        Search for voices in the Voice Library.
        
        Args:
            query: Search query
            gender: Filter by gender ('male', 'female')
            age: Filter by age group ('young', 'middle-aged', 'old')
            accent: Filter by accent
            language: Filter by language
            use_cases: Filter by use cases (e.g., ['narration', 'characters'])
            page_size: Number of results per page
            
        Returns:
            Dictionary with voices and pagination info
        """
        print("Searching Voice Library...")
        if query:
            print(f"Query: {query}")
        
        result = self.api.get_shared_voices(
            page_size=page_size,
            gender=gender,
            age=age,
            accent=accent,
            language=language,
            search=query,
            use_cases=use_cases
        )
        
        return result
    
    def display_voices(
        self,
        voices: List[Dict[str, Any]],
        show_details: bool = False
    ) -> None:
        """
        Display voice information in a formatted way.
        
        Args:
            voices: List of voice dictionaries
            show_details: Show detailed information
        """
        print(f"\n{'='*80}")
        print(f"Found {len(voices)} voice(s)")
        print(f"{'='*80}\n")
        
        for i, voice in enumerate(voices, 1):
            print(f"{i}. {voice.get('name', 'Unknown')}")
            print(f"   Voice ID: {voice.get('public_owner_id', 'N/A')}/{voice.get('voice_id', 'N/A')}")
            
            if 'description' in voice:
                print(f"   Description: {voice['description']}")
            
            if 'labels' in voice:
                labels = voice['labels']
                label_str = ', '.join([f"{k}: {v}" for k, v in labels.items()])
                print(f"   Labels: {label_str}")
            
            if 'preview_url' in voice:
                print(f"   Preview: {voice['preview_url']}")
            
            if 'creator_name' in voice:
                print(f"   Creator: {voice['creator_name']}")
            
            if show_details:
                if 'category' in voice:
                    print(f"   Category: {voice['category']}")
                if 'use_cases' in voice:
                    print(f"   Use Cases: {', '.join(voice['use_cases'])}")
            
            print()
    
    def add_voice_to_account(
        self,
        public_user_id: str,
        voice_id: str,
        new_name: str
    ) -> Dict[str, Any]:
        """
        Add a voice from the library to your account.
        
        Args:
            public_user_id: Public user ID of the voice creator
            voice_id: Voice ID to add
            new_name: Name for the voice in your account
            
        Returns:
            Added voice information
        """
        print(f"Adding voice '{new_name}' to your account...")
        
        result = self.api.add_voice_from_library(
            public_user_id=public_user_id,
            voice_id=voice_id,
            new_name=new_name
        )
        
        print(f"✓ Voice added successfully!")
        print(f"  Voice ID: {result.get('voice_id', 'N/A')}")
        print(f"  Name: {new_name}")
        
        return result
    
    def get_featured_voices(self) -> Dict[str, Any]:
        """
        Get featured voices from the library.
        
        Returns:
            Dictionary with featured voices
        """
        # Get high-quality professional voices
        return self.search_voices(
            use_cases=['narration', 'audiobook'],
            page_size=20
        )
    
    def get_voices_by_category(self, category: str) -> Dict[str, Any]:
        """
        Get voices filtered by category.
        
        Args:
            category: Category to filter by
            
        Returns:
            Dictionary with voices in the category
        """
        return self.api.get_shared_voices(
            category=category,
            page_size=30
        )
    
    def browse_interactive(self) -> None:
        """
        Interactive voice library browser.
        """
        print("\n" + "="*80)
        print("  ElevenLabs Voice Library Browser")
        print("="*80)
        
        while True:
            print("\nOptions:")
            print("  1. Search voices")
            print("  2. Filter by gender")
            print("  3. Filter by use case")
            print("  4. Browse featured voices")
            print("  5. Add voice to account")
            print("  6. Exit")
            
            choice = input("\nSelect option (1-6): ").strip()
            
            if choice == '1':
                query = input("Enter search query: ").strip()
                result = self.search_voices(query=query)
                self.display_voices(result.get('voices', []))
            
            elif choice == '2':
                gender = input("Gender (male/female): ").strip()
                result = self.search_voices(gender=gender)
                self.display_voices(result.get('voices', []))
            
            elif choice == '3':
                print("Common use cases: narration, characters, audiobook, gaming")
                use_case = input("Enter use case: ").strip()
                result = self.search_voices(use_cases=[use_case])
                self.display_voices(result.get('voices', []))
            
            elif choice == '4':
                result = self.get_featured_voices()
                self.display_voices(result.get('voices', []), show_details=True)
            
            elif choice == '5':
                public_user_id = input("Public User ID: ").strip()
                voice_id = input("Voice ID: ").strip()
                new_name = input("Name for this voice: ").strip()
                self.add_voice_to_account(public_user_id, voice_id, new_name)
            
            elif choice == '6':
                print("\nExiting...")
                break
            
            else:
                print("Invalid option")


def main():
    """Command-line interface for voice library."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Browse ElevenLabs Voice Library')
    parser.add_argument('-s', '--search', help='Search query')
    parser.add_argument('-g', '--gender', help='Filter by gender (male/female)')
    parser.add_argument('-a', '--age', help='Filter by age group')
    parser.add_argument('-l', '--language', help='Filter by language')
    parser.add_argument('--use-case', help='Filter by use case')
    parser.add_argument('--featured', action='store_true', help='Show featured voices')
    parser.add_argument('--interactive', action='store_true', help='Interactive browser')
    parser.add_argument('--add', nargs=3, metavar=('USER_ID', 'VOICE_ID', 'NAME'), 
                       help='Add voice to account')
    
    args = parser.parse_args()
    
    try:
        browser = VoiceLibraryBrowser()
        
        if args.interactive:
            browser.browse_interactive()
            return
        
        if args.add:
            browser.add_voice_to_account(args.add[0], args.add[1], args.add[2])
            return
        
        # Search or filter
        use_cases = [args.use_case] if args.use_case else None
        
        if args.featured:
            result = browser.get_featured_voices()
        else:
            result = browser.search_voices(
                query=args.search,
                gender=args.gender,
                age=args.age,
                language=args.language,
                use_cases=use_cases
            )
        
        browser.display_voices(result.get('voices', []), show_details=True)
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
